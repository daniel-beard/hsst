module Prims
  ( primSchemes,
    primNames,
    lookupImpl,
  )
where

import Control.Monad (filterM, (>=>))
import Control.Monad.RWS.Strict (get, put, tell)
import Core
import Data.Array (elems)
import qualified Data.ByteString.Base64 as B64
import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.ICU.Char as ICU
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality ((:~:) (Refl))
import Eval (render)
import Infer (Scheme (..))
import Interp (Interp, InterpS(..))
import Syntax
import Text.Printf
import Text.Regex.PCRE (Regex, matchOnceText, matchTest)

-- Type-variable identifiers used inside primitive schemes. They live in
-- the same TVar namespace as inference; we just pick small numbers and
-- let `instantiate` rename them on every use.
a, b, c :: TVar
a = 0
b = 1
c = 2

-- One row per primitive: name, type scheme (used by inference), and a
-- dispatcher that returns the implementation iff the requested monomorphic type matches.
-- Arrows are Kleisli arrow - see `Interp`
data Prim = Prim
  { primName_ :: Name,
    primScheme_ :: Scheme,
    primImpl_ :: forall t. Ty t -> Maybe t
  }

-- Monomorphic-prim helper: dispatcher that succeeds only when the
-- requested type matches the impl's known type.
mono :: forall s. Ty s -> s -> (forall t. Ty t -> Maybe t)
mono expected impl reqTy = case cmpTy expected reqTy of
  Just Refl -> Just impl
  Nothing   -> Nothing

-- Build a monomorphic primitive from a single source of truth: its concrete `Ty`. 
-- The inference Scheme is derived from it (no quantified variables),
-- and the dispatcher is `mono` against the same type, so the type is written exactly once.
monoPrim :: forall s. Name -> Ty s -> s -> Prim
monoPrim name ty impl =
  Prim name (monoScheme ty) (mono ty impl)

-- Inference scheme for a monomorphic type. No qualitifed variables here.
monoScheme :: Ty s -> Scheme
monoScheme ty = Scheme [] (tyToUType ty)

-- Lift pure Haskell function into kleisli arrow that runs no effects.
-- E.g. `words` is an ordinary curried func, which needs to be wrapped.
-- `k1` & `k2` wraps the func so it matches the shape the runtime expects: `(a -> Interp b)`
-- TODO: Readdress if this gets annoying
k1 :: (a -> b) -> (a -> Interp b)
k1 f = pure . f

k2 :: (a -> b -> c) -> (a -> Interp (b -> Interp c))
k2 f x = pure $ k1 $ f x

-- (a -> b) -> (b -> c) -> (a -> c). Shared by `compose` and the `|>` operator.
composeScheme :: Scheme
composeScheme =
  Scheme [a, b, c] $
    (TyVar a `TyArr` TyVar b)
      `TyArr` ((TyVar b `TyArr` TyVar c) `TyArr` (TyVar a `TyArr` TyVar c))

prims :: [Prim]
prims =
  -- Pipeline plumbing
  [ Prim "compose" composeScheme implCompose,
    -- `|>` is the same function as compose; separate for inference diagnostics.
    -- Not a valid identifier, can only ever be emitted by the parser desugaring `|>`.
    Prim "|>" composeScheme implCompose,
    -- tee: render the current value (Show-style, to the Writer) and pass it through.
    Prim
      "tee"
      (Scheme [a] $ TyVar a `TyArr` TyVar a) implTee,
    -- String <-> [String]. A String is [Char]. The polymorphic list ops below also apply.
    monoPrim "words"      (tyStr :-> TyListT tyStr) (k1 words),
    monoPrim "unwords"    (TyListT tyStr :-> tyStr) (k1 unwords),
    monoPrim "lines"      (tyStr :-> TyListT tyStr) (k1 lines),
    monoPrim "unlines"    (TyListT tyStr :-> tyStr) (k1 unlines),
    -- Scalar string ops
    monoPrim "uppercase"  (tyStr :-> tyStr) (k1 $ map toUpper),
    monoPrim "lowercase"  (tyStr :-> tyStr) (k1 $ map toLower),
    monoPrim "inspect"    (tyStr :-> TyListT tyStr) (k1 $ map ICU.charName),
    -- Char ops -- pair these with map/filter to work per character
    monoPrim "upcaseChar"   (TyCharT :-> TyCharT) (k1 toUpper),
    monoPrim "downcaseChar" (TyCharT :-> TyCharT) (k1 toLower),
    -- Unicode character name, e.g. 'a' -> "LATIN SMALL LETTER A".
    -- "" for unnamed code points.
    monoPrim "charName"   (TyCharT :-> tyStr) (k1 ICU.charName),
    monoPrim "codePoint"  (TyCharT :-> tyStr) (k1 $ printf "U+%04X"),
    monoPrim "base64"     (tyStr :-> tyStr) (k1 b64encode),
    monoPrim "unbase64"   (tyStr :-> tyStr) (k1 b64decode),
    -- Regex ops
    -- Regex first so a partial application `matches(/foo/)` is a String -> Bool
    monoPrim "matches" (TyRegexT :-> tyStr :-> TyBoolT) (k2 matchTest),
    -- match - runs the regex, records capture groups in interpreter State. 
    -- returns whole match or "" if no match
    Prim "match" (monoScheme matchTy) implMatch,
    -- group: retrieves a previous capture group recorded by the most recent `match`
    Prim "group" (monoScheme groupTy) implGroup,
    -- TODO: Remove these, dollar refs should desugar to group(n) directly
    Prim "$1" (monoScheme dollarTy) (implDollar 1),
    Prim "$2" (monoScheme dollarTy) (implDollar 2),
    Prim
      "take"
      -- Int -> [a] -> [a]
      (Scheme [a] $ TyInt `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a)))
      implTake,
    Prim
      "drop"
      -- Int -> [a] -> [a]
      (Scheme [a] $ TyInt `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a)))
      implDrop,
    Prim
      "length"
      -- [a] -> Int
      (Scheme [a] $ TyList (TyVar a) `TyArr` TyInt)
      implLength,
    Prim
      "reverse"
      -- [a] -> [a]
      (Scheme [a] $ TyList (TyVar a) `TyArr` TyList (TyVar a))
      implReverse,
    Prim
      "map"
      -- (a -> b) -> [a] -> [b]
      (Scheme [a, b] $
          (TyVar a `TyArr` TyVar b)
            `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar b))
      )
      implMap,
    Prim
      "filter"
      -- (a -> b) -> [a] -> [a]
      (Scheme [a] $
          (TyVar a `TyArr` TyBool)
            `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a))
      )
      implFilter,
    -- Int ops
    monoPrim "plus" (TyIntT :-> (TyIntT :-> TyIntT)) (k2 (+)),
    monoPrim "minus" (TyIntT :-> (TyIntT :-> TyIntT)) (k2 (-)),
    -- Bool ops
    monoPrim "not" (TyBoolT :-> TyBoolT) (k1 Prelude.not)
  ]

primSchemes :: [(Name, Scheme)]
primSchemes = [(primName_ p, primScheme_ p) | p <- prims]

primNames :: Set Name
primNames = Set.fromList (map primName_ prims)

lookupImpl :: forall t. Name -> Ty t -> Maybe t
lookupImpl name reqTy = go prims
  where
    go [] = Nothing
    go (p : rest)
      | primName_ p == name = primImpl_ p reqTy
      | otherwise = go rest

b64encode :: String -> String
b64encode = T.unpack . TE.decodeUtf8 . B64.encode . TE.encodeUtf8 . T.pack

b64decode :: String -> String
b64decode s = case B64.decode (TE.encodeUtf8 (T.pack s)) of
  Right bs -> T.unpack (TE.decodeUtf8 bs)
  Left e   -> error $ "unbase64: " ++ e

-- Concrete types for the regex group prims (also the single source for their inference schemes via `monoScheme`). 
-- The index is the Kleisli runtime type: each source-level arrow contributes an `Interp` in the value's type.
matchTy :: Ty (Regex -> Interp (String -> Interp String))
matchTy = TyRegexT :-> tyStr :-> tyStr

groupTy :: Ty (Int -> Interp (String -> Interp String))
groupTy = TyIntT :-> tyStr :-> tyStr

-- `$1`/`$2` are `group` with the index already supplied: just `String -> String`.
dollarTy :: Ty (String -> Interp String)
dollarTy = tyStr :-> tyStr

-- Polymorphic-prim dispatchers. 
-- Each pattern-matches the requested Ty to recover the witnesses for the type variables, 
-- then returns a fully monomorphic runtime value.
-- Arrows in the result are kleisli arrows in `Interp`
-- (`compose` becomes kleisli composition, `map` becomes `mapM`, etc.)

implCompose :: forall t. Ty t -> Maybe t
implCompose ((a1 :-> b1) :-> (b2 :-> c1) :-> a2 :-> c2) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy b1 b2
    Refl <- cmpTy c1 c2
    pure (\f -> pure (\g -> pure $ f >=> g))
implCompose _ = Nothing

implMap :: forall t. Ty t -> Maybe t
implMap ((a1 :-> b1) :-> TyListT a2 :-> TyListT b2) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy b1 b2
    pure (\f -> pure (\xs -> mapM f xs))
implMap _ = Nothing

implFilter :: forall t. Ty t -> Maybe t
implFilter ((a1 :-> TyBoolT) :-> TyListT a2 :-> TyListT a3) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy a1 a3
    pure (\f -> pure (\xs -> filterM f xs))
implFilter _ = Nothing

implTake :: forall t. Ty t -> Maybe t
implTake (TyIntT :-> TyListT a1 :-> TyListT a2) = do
  Refl <- cmpTy a1 a2
  pure (\n -> pure (\xs -> pure (take n xs)))
implTake _ = Nothing

implDrop :: forall t. Ty t -> Maybe t
implDrop (TyIntT :-> TyListT a1 :-> TyListT a2) = do
  Refl <- cmpTy a1 a2
  pure (\n -> pure (pure . drop n))
implDrop _ = Nothing

implLength :: forall t. Ty t -> Maybe t
implLength (TyListT _a :-> TyIntT) = Just (\xs -> pure (length xs))
implLength _ = Nothing

implReverse :: forall t. Ty t -> Maybe t
implReverse (TyListT a1 :-> TyListT a2) = do
  Refl <- cmpTy a1 a2
  pure (\xs -> pure $ reverse xs)
implReverse _ = Nothing

-- tee: a -> a, writes the rendered value back into the InterpW
-- TODO: Could be actual IO in future, but pure for now & rendered later.
implTee :: forall t. Ty t -> Maybe t
implTee (a1 :-> a2) = do
  Refl <- cmpTy a1 a2
  pure (\x -> do tell [render a1 x]; pure x)
implTee _ = Nothing

-- match: store the capture groups of the first match in State, return group 0.
implMatch :: forall t. Ty t -> Maybe t
implMatch ty = do
  Refl <- cmpTy ty matchTy
  pure (\rx -> pure (\s -> do
    let groups = matchGroups rx s
    put (InterpS groups)
    pure (headOr "" groups)))

-- group: read the n-th group recorded by the most recent `match`, ignoring
-- the piped-in value (so it slots into a `|>` pipeline after `match`).
implGroup :: forall t. Ty t -> Maybe t
implGroup ty = do
  Refl <- cmpTy ty groupTy
  pure (\n -> pure (\_ -> readGroup n))

-- $1 / $2: read a fixed group index, ignoring the piped-in value.
implDollar :: forall t. Int -> Ty t -> Maybe t
implDollar n ty = do
  Refl <- cmpTy ty dollarTy
  pure (\_ -> readGroup n)

-- Read the n-th group recorded by the most recent `match`, "" if out of range.
readGroup :: Int -> Interp String
readGroup n = do
  InterpS gs <- get
  pure (if n >= 0 && n < length gs then gs !! n else "")

-- All matched substrings of the first match, empty when pattern doesn't match.
-- 0: the whole match
-- 1.. the matched groups
matchGroups :: Regex -> String -> [String]
matchGroups rx s = case matchOnceText rx s of
  Just (_, arr, _) -> map fst (elems arr)
  Nothing          -> []

--TODO: Move this somewhere else.
headOr :: a -> [a] -> a
headOr d []      = d
headOr _ (x : _) = x
