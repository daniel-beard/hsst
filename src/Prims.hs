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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.ICU as ICUB
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

--TODO: Once String type is no longer a collection, these don't have to be in overload order anymore.
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
    monoPrim "words"      (TyStrT :-> TyListT TyStrT) (k1 T.words),
    monoPrim "unwords"    (TyListT TyStrT :-> TyStrT) (k1 T.unwords),
    monoPrim "lines"      (TyStrT :-> TyListT TyStrT) (k1 T.lines),
    monoPrim "unlines"    (TyListT TyStrT :-> TyStrT) (k1 T.unlines),
    -- Scalar string ops
    monoPrim "uppercase"  (TyStrT :-> TyStrT) (k1 $ T.toUpper),
    monoPrim "lowercase"  (TyStrT :-> TyStrT) (k1 $ T.toLower),
    monoPrim "inspect"    (TyStrT :-> TyListT TyStrT) (k1 $ map (T.pack . ICU.charName) . T.unpack),
    -- Char ops -- pair these with map/filter to work per character
    monoPrim "upcaseChar"   (TyCharT :-> TyCharT) (k1 toUpper),
    monoPrim "downcaseChar" (TyCharT :-> TyCharT) (k1 toLower),
    -- Unicode character name, e.g. 'a' -> "LATIN SMALL LETTER A".
    -- "" for unnamed code points.
    monoPrim "charName"   (TyCharT :-> TyStrT) (k1 (T.pack . ICU.charName)),
    monoPrim "codePoint"  (TyCharT :-> TyStrT) (k1 $ T.pack . printf "U+%04X"),
    monoPrim "base64"     (TyStrT :-> TyStrT) (k1 b64encode),
    monoPrim "unbase64"   (TyStrT :-> TyStrT) (k1 b64decode),
    -- Regex ops
    -- Regex first so a partial application `matches(/foo/)` is a String -> Bool
    monoPrim "matches" (TyRegexT :-> TyStrT :-> TyBoolT) (k2 (\rx s -> matchTest rx (T.unpack s))),
    -- match - runs the regex, records capture groups in interpreter State. 
    -- returns whole match or "" if no match
    Prim "match" (monoScheme matchTy) implMatch,
    -- group: retrieves a previous capture group recorded by the most recent `match`
    Prim "group" (monoScheme groupTy) implGroup,
    -- TODO: Remove these, dollar refs should desugar to group(n) directly
    Prim "$1" (monoScheme dollarTy) (implDollar 1),
    Prim "$2" (monoScheme dollarTy) (implDollar 2),
    monoPrim "take" (TyIntT :-> TyStrT :-> TyStrT) (k2 T.take),
    Prim
      "take"
      -- Int -> [a] -> [a]
      (Scheme [a] $ TyInt `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a)))
      implTake,
    monoPrim "drop" (TyIntT :-> TyStrT :-> TyStrT) (k2 T.drop),
    Prim
      "drop"
      -- Int -> [a] -> [a]
      (Scheme [a] $ TyInt `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a)))
      implDrop,
    monoPrim "length" (TyStrT :-> TyIntT) (k1 T.length),
    Prim
      "length"
      -- [a] -> Int
      (Scheme [a] $ TyList (TyVar a) `TyArr` TyInt)
      implLength,
    -- Grapheme cluster aware reverse specialized for String
    monoPrim "reverse" (TyStrT :-> TyStrT) (k1 graphemeReverse),
    -- `[a] -> [a]` element-wise reverse
    Prim
      "reverse"
      (Scheme [a] $ TyList (TyVar a) `TyArr` TyList (TyVar a))
      implReverse,
    -- Char-wise map/filter specialized for String.
    -- Char ops are Kleisli - have to thread the effect with mapM/filterM
    monoPrim "map"
      ((TyCharT :-> TyCharT) :-> TyStrT :-> TyStrT)
      (\f -> pure (\s -> T.pack <$> mapM f (T.unpack s))),
    Prim
      "map"
      -- (a -> b) -> [a] -> [b]
      (Scheme [a, b] $
          (TyVar a `TyArr` TyVar b)
            `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar b))
      )
      implMap,
    monoPrim "filter"
      ((TyCharT :-> TyBoolT) :-> TyStrT :-> TyStrT)
      (\p -> pure (\s -> T.pack <$> filterM p (T.unpack s))),
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

-- Map of names to type scheme lists, so prims can be overloaded.
primSchemes :: Map Name [Scheme]
primSchemes = Map.fromListWith (flip (++)) [(primName_ p, [primScheme_ p]) | p <- prims]

primNames :: Set Name
primNames = Set.fromList (map primName_ prims)

-- Find an implementation for the given name that matches the requested runtime type.
-- Fallthrough to next when no match.
lookupImpl :: forall t. Name -> Ty t -> Maybe t
lookupImpl name reqTy = go prims
  where
    go [] = Nothing
    go (p : rest)
      | primName_ p == name = case primImpl_ p reqTy of
          Just impl -> Just impl
          Nothing   -> go rest
      | otherwise = go rest

-- Reverse a string by grapheme cluster rather than by Char
graphemeReverse :: T.Text -> T.Text
graphemeReverse =
  mconcat
    . reverse
    . map ICUB.brkBreak
    . ICUB.breaks (ICUB.breakCharacter ICUB.Current)

b64encode :: T.Text -> T.Text
b64encode = TE.decodeUtf8 . B64.encode . TE.encodeUtf8

b64decode :: T.Text -> T.Text
b64decode s = case B64.decode (TE.encodeUtf8 s) of
  Right bs -> TE.decodeUtf8 bs
  Left e   -> error $ "unbase64: " ++ e

-- Concrete types for the regex group prims (also the single source for their inference schemes via `monoScheme`). 
-- The index is the Kleisli runtime type: each source-level arrow contributes an `Interp` in the value's type.
matchTy :: Ty (Regex -> Interp (T.Text -> Interp T.Text))
matchTy = TyRegexT :-> TyStrT :-> TyStrT

groupTy :: Ty (Int -> Interp (T.Text -> Interp T.Text))
groupTy = TyIntT :-> TyStrT :-> TyStrT

-- `$1`/`$2` are `group` with the index already supplied: just `String -> String`.
dollarTy :: Ty (T.Text -> Interp T.Text)
dollarTy = TyStrT :-> TyStrT

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
    pure $ k2 (>=>)
implCompose _ = Nothing

implMap :: forall t. Ty t -> Maybe t
implMap ((a1 :-> b1) :-> TyListT a2 :-> TyListT b2) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy b1 b2
    pure $ k1 mapM
implMap _ = Nothing

implFilter :: forall t. Ty t -> Maybe t
implFilter ((a1 :-> TyBoolT) :-> TyListT a2 :-> TyListT a3) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy a1 a3
    pure $ k1 filterM
implFilter _ = Nothing

implTake :: forall t. Ty t -> Maybe t
implTake (TyIntT :-> TyListT a1 :-> TyListT a2) = do
  Refl <- cmpTy a1 a2
  pure $ k2 take
implTake _ = Nothing

implDrop :: forall t. Ty t -> Maybe t
implDrop (TyIntT :-> TyListT a1 :-> TyListT a2) = do
  Refl <- cmpTy a1 a2
  pure $ k2 drop
implDrop _ = Nothing

implLength :: forall t. Ty t -> Maybe t
implLength (TyListT _a :-> TyIntT) = Just $ k1 length
implLength _ = Nothing

implReverse :: forall t. Ty t -> Maybe t
implReverse (TyListT a1 :-> TyListT a2) = do
  Refl <- cmpTy a1 a2
  pure $ k1 reverse
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
readGroup :: Int -> Interp T.Text
readGroup n = do
  InterpS gs <- get
  pure (if n >= 0 && n < length gs then gs !! n else "")

-- All matched substrings of the first match, empty when pattern doesn't match.
-- 0: the whole match
-- 1.. the matched groups
matchGroups :: Regex -> T.Text -> [T.Text]
matchGroups rx s = case matchOnceText rx (T.unpack s) of
  Just (_, arr, _) -> map (T.pack . fst) (elems arr)
  Nothing          -> []

--TODO: Move this somewhere else.
headOr :: a -> [a] -> a
headOr d []      = d
headOr _ (x : _) = x
