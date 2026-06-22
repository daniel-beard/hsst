{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Core
  ( Ty (..),
    Term (..),
    Var (..),
    Typed (..),
    ExType (..),
    tyStr,
    cmpTy,
    reifyTy,
    tyToUType,
    showTy,
  )
where

import Data.Type.Equality ((:~:) (Refl))
import Text.Regex.PCRE (Regex)
import Syntax (UType (..), prettyUType)

-- Strongly-typed types. Each constructor's parameter index is a real Haskell
-- type, so a `Ty t` is a singleton witness for `t`.
data Ty t where
  TyCharT  :: Ty Char
  TyIntT   :: Ty Int
  TyBoolT  :: Ty Bool
  -- A compiled PCRE regex.
  TyRegexT :: Ty Regex
  -- [a]
  TyListT  :: Ty a -> Ty [a]
  -- a -> b
  TyArrT   :: Ty a -> Ty b -> Ty (a -> b)

deriving instance Show (Ty t)

-- The string type: [Char]. List operations work on these, like `length`
tyStr :: Ty String
tyStr = TyListT TyCharT

-- de Bruijn index, type-aware.
data Var g t where
  ZVar :: Var (g, t) t
  SVar :: Var g t -> Var (g, s) t

-- The GADT core. Polymorphism has been eliminated by inference, so every
-- node carries a concrete type.
data Term g t where
  TVar  :: Var g t -> Term g t
  TLam  :: Ty a -> Term (g, a) b -> Term g (a -> b)
  TApp  :: Term g (a -> b) -> Term g a -> Term g b
  TStr   :: String -> Term g String
  TRegex :: Regex -> Term g Regex
  TChar  :: Char -> Term g Char
  TInt   :: Int -> Term g Int
  TBool  :: Bool -> Term g Bool
  -- Primitives are opaque Haskell values of the right type.
  TPrim :: String -> Ty t -> t -> Term g t

-- Existential wrappers for "we have some Ty/Term but its index is hidden".
data Typed thing = forall t. Typed (Ty t) (thing t)

data ExType = forall t. ExType (Ty t)

cmpTy :: Ty a -> Ty b -> Maybe (a :~: b)
cmpTy TyCharT TyCharT   = Just Refl
cmpTy TyIntT TyIntT     = Just Refl
cmpTy TyBoolT TyBoolT   = Just Refl
cmpTy TyRegexT TyRegexT = Just Refl
cmpTy (TyListT a) (TyListT b) = do
  Refl <- cmpTy a b
  pure Refl
cmpTy (TyArrT a1 b1) (TyArrT a2 b2) = do
  Refl <- cmpTy a1 a2
  Refl <- cmpTy b1 b2
  pure Refl
cmpTy _ _ = Nothing

-- Reify a (now-monomorphic) UType into the GADT-singleton form.
-- This is the bridge from inference's untyped UType to the GADT.
reifyTy :: UType -> Either String ExType
reifyTy ut = case ut of
  TyChar    -> Right (ExType TyCharT)
  TyInt     -> Right (ExType TyIntT)
  TyBool    -> Right (ExType TyBoolT)
  TyRegex   -> Right (ExType TyRegexT)
  TyList a  -> do
    ExType a' <- reifyTy a
    Right (ExType (TyListT a'))
  TyArr a b -> do
    ExType a' <- reifyTy a
    ExType b' <- reifyTy b
    Right (ExType (TyArrT a' b'))
  TyVar n   ->
    Left $
      "ambiguous type: free type variable t"
        ++ show n
        ++ " survived inference (the program is polymorphic at the top level "
        ++ "and would need an annotation to run)"

-- The inverse of `reifyTy` for monomorphic types: project a GADT singleton
-- back into the untyped UType. A `Ty t` never contains type variables, so
-- this is total. Lets a monomorphic primitive declare its type once (as a
-- `Ty`) and derive its inference Scheme from it.
tyToUType :: Ty t -> UType
tyToUType t = case t of
  TyCharT    -> TyChar
  TyIntT     -> TyInt
  TyBoolT    -> TyBool
  TyRegexT   -> TyRegex
  TyListT a  -> TyList (tyToUType a)
  TyArrT a b -> TyArr (tyToUType a) (tyToUType b)

-- Render a GADT type by projecting to UType and reusing the single
-- UType pretty-printer. (Arrows are now precedence-parenthesized rather
-- than always wrapped.)
showTy :: Ty t -> String
showTy = prettyUType . tyToUType
