{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prims
  ( primSchemes,
    primNames,
    lookupImpl,
  )
where

import Core
import qualified Data.ByteString.Base64 as B64
import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Type.Equality ((:~:) (Refl))
import Eval (render)
import Infer (Scheme (..))
import Syntax
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

-- Type-variable identifiers used inside primitive schemes. They live in
-- the same TVar namespace as inference; we just pick small numbers and
-- let `instantiate` rename them on every use.
a, b, c :: TVar
a = 0
b = 1
c = 2

-- One row per primitive: name, type scheme (used by inference), and a
-- dispatcher that returns the implementation iff the requested
-- monomorphic type matches.
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

-- Build a monomorphic primitive from a single source of truth: its concrete
-- `Ty`. The inference Scheme is derived from it (no quantified variables),
-- and the dispatcher is `mono` against the same type, so the type is written
-- exactly once.
monoPrim :: forall s. Name -> Ty s -> s -> Prim
monoPrim name ty impl =
  Prim name (Scheme [] (tyToUType ty)) (mono ty impl)

prims :: [Prim]
prims =
  -- Pipeline plumbing
  [ Prim
      "compose"
      ( Scheme [a, b, c] $
          (TyVar a `TyArr` TyVar b)
            `TyArr` ((TyVar b `TyArr` TyVar c) `TyArr` (TyVar a `TyArr` TyVar c))
      )
      implCompose,
    -- tee: render the current value (Show-style, to stdout) and pass it through.
    Prim
      "tee"
      (Scheme [a] $ TyVar a `TyArr` TyVar a) implTee,
    -- String <-> [String]. A String is [Char]. The polymorphic list ops below also apply.
    monoPrim "words"      (tyStr `TyArrT` (TyListT tyStr)) words,
    monoPrim "unwords"    ((TyListT tyStr) `TyArrT` tyStr) unwords,
    monoPrim "lines"      (tyStr `TyArrT` (TyListT tyStr)) lines,
    monoPrim "unlines"    ((TyListT tyStr) `TyArrT` tyStr) unlines,
    -- Scalar string ops
    monoPrim "uppercase"  (tyStr `TyArrT` tyStr) (map toUpper),
    monoPrim "lowercase"  (tyStr `TyArrT` tyStr) (map toLower),
    -- Char ops -- pair these with map/filter to work per character
    monoPrim "upcaseChar"   (TyCharT `TyArrT` TyCharT) toUpper,
    monoPrim "downcaseChar" (TyCharT `TyArrT` TyCharT) toLower,
    monoPrim "base64"     (tyStr `TyArrT` tyStr) b64encode,
    monoPrim "unbase64"   (tyStr `TyArrT` tyStr) b64decode,
    Prim
      "take"
      -- Int -> [a] -> [a]
      (Scheme [a] $ TyInt `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a)))
      implTake,
    Prim
      "drop"
      (Scheme [a] $ TyInt `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a)))
      implDrop,
    Prim
      "length"
      (Scheme [a] $ TyList (TyVar a) `TyArr` TyInt)
      implLength,
    Prim
      "reverse"
      (Scheme [a] $ TyList (TyVar a) `TyArr` TyList (TyVar a))
      implReverse,
    Prim
      "map"
      ( Scheme [a, b] $
          (TyVar a `TyArr` TyVar b)
            `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar b))
      )
      implMap,
    Prim
      "filter"
      ( Scheme [a] $
          (TyVar a `TyArr` TyBool)
            `TyArr` (TyList (TyVar a) `TyArr` TyList (TyVar a))
      )
      implFilter,
    -- Int ops
    -- These are curried, so partial application already works: plus(2, 3),
    -- plus(2)(3), and `length |> plus(2)` (compose of [a]->Int and
    -- Int->Int) all work. `4 |> plus(2)` does NOT, because `|>` is function
    -- composition, not a value pipe -- 4 is a value, not a function.
    monoPrim "plus" (TyArrT TyIntT (TyArrT TyIntT TyIntT)) (+),
    monoPrim "minus" (TyArrT TyIntT (TyArrT TyIntT TyIntT)) (-),
    -- Bool ops
    monoPrim "not" (TyBoolT `TyArrT` TyBoolT) Prelude.not
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

-- Polymorphic-prim dispatchers. Each pattern-matches the requested Ty
-- to recover the witnesses for the type variables, then returns a fully
-- monomorphic Haskell value.

implCompose :: forall t. Ty t -> Maybe t
implCompose
  (TyArrT
      (TyArrT a1 b1)
      (TyArrT
          (TyArrT b2 c1)
          (TyArrT a2 c2)
       )
   ) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy b1 b2
    Refl <- cmpTy c1 c2
    pure (\f g x -> g (f x))
implCompose _ = Nothing

implMap :: forall t. Ty t -> Maybe t
implMap
  (TyArrT
      (TyArrT a1 b1)
      (TyArrT (TyListT a2) (TyListT b2))
   ) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy b1 b2
    pure map
implMap _ = Nothing

implFilter :: forall t. Ty t -> Maybe t
implFilter
  ( TyArrT
      (TyArrT a1 TyBoolT)
      (TyArrT (TyListT a2) (TyListT a3))
    ) = do
    Refl <- cmpTy a1 a2
    Refl <- cmpTy a1 a3
    pure filter
implFilter _ = Nothing

implTake :: forall t. Ty t -> Maybe t
implTake (TyArrT TyIntT (TyArrT (TyListT a1) (TyListT a2))) = do
  Refl <- cmpTy a1 a2
  pure take
implTake _ = Nothing

implDrop :: forall t. Ty t -> Maybe t
implDrop (TyArrT TyIntT (TyArrT (TyListT a1) (TyListT a2))) = do
  Refl <- cmpTy a1 a2
  pure drop
implDrop _ = Nothing

implLength :: forall t. Ty t -> Maybe t
implLength (TyArrT (TyListT _aTy) TyIntT) = Just length
implLength _ = Nothing

implReverse :: forall t. Ty t -> Maybe t
implReverse (TyArrT (TyListT a1) (TyListT a2)) = do
  Refl <- cmpTy a1 a2
  pure reverse
implReverse _ = Nothing

-- tee: a -> a, with a side-effect of rendering the value to stderr
-- as it flows through. We need the runtime witness `Ty a` for `render`,
-- which is exactly what destructuring the requested type hands us.
-- (unsafePerformIO + NOINLINE is the same trick Debug.Trace uses; the
-- pure interpreter stays pure-typed but gets to leak a printf.)
implTee :: forall t. Ty t -> Maybe t
implTee (TyArrT a1 a2) = do
  Refl <- cmpTy a1 a2
  pure (teeImpl a1)
implTee _ = Nothing

{-# NOINLINE teeImpl #-}
teeImpl :: Ty a -> a -> a
teeImpl ty x = unsafePerformIO $ do
  putStrLn (render ty x)
  hFlush stdout
  pure x
