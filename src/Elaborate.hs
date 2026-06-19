{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Elaborate
  ( TyEnv(..)
  , elaborate
  , elaborateClosed
  ) where

import Data.Type.Equality ((:~:)(Refl))

import Syntax
import Core
import qualified Prims as Prims

-- The elaboration-time type environment: a stack of GADT singleton types
-- mirroring the de Bruijn lambda-binder stack. Indexed by the same `g`
-- parameter as Term/Var, so the type-equality witness produced for an
-- IVar lookup is exactly the witness Term needs.
data TyEnv g where
  TyNil  :: TyEnv ()
  TyCons :: Ty t -> TyEnv g -> TyEnv (g, t)

-- Look up the i-th binder's GADT type, returning a Typed Var witness.
lookupVar :: Int -> TyEnv g -> Either String (Typed (Var g))
lookupVar _ TyNil = Left "internal: dangling de Bruijn index in elaboration"
lookupVar 0 (TyCons ty _)  = Right (Typed ty ZVar)
lookupVar n (TyCons _  rest) = do
  Typed ty v <- lookupVar (n - 1) rest
  Right (Typed ty (SVar v))

-- Elaborate an annotated, monomorphic term into a Typed (Term g), using
-- the inferred annotations to drive both reifyTy (UType -> Ty) and
-- cmpTy-style equality checks.
elaborate :: TyEnv g -> AnnTerm -> Either String (Typed (Term g))
elaborate env e = case e of
  AStr  v -> Right (Typed TyStr   (TStr v))
  AInt  v -> Right (Typed TyIntT  (TInt v))
  ABool v -> Right (Typed TyBoolT (TBool v))

  AVar i ty -> do
    Typed ty' v <- lookupVar i env
    -- The annotation should match; cmpTy gives us the runtime witness.
    expected <- reifyTy ty
    case expected of
      ExType ety -> case cmpTy ety ty' of
        Just Refl -> Right (Typed ty' (TVar v))
        Nothing   -> Left $
          "internal: AVar annotation " ++ prettyUType ty
          ++ " disagrees with elaboration env type " ++ showTy ty'

  APrim n ty -> do
    ExType rty <- reifyTy ty
    case Prims.lookupImpl n rty of
      Just impl -> Right (Typed rty (TPrim n rty impl))
      Nothing   -> Left $
        "primitive " ++ n ++ " has no implementation at type " ++ showTy rty

  ALam binderUTy body -> do
    ExType bTy <- reifyTy binderUTy
    Typed retTy body' <- elaborate (TyCons bTy env) body
    Right (Typed (TyArrT bTy retTy) (TLam bTy body'))

  AApp f a -> do
    Typed tF fTerm <- elaborate env f
    Typed tA aTerm <- elaborate env a
    case tF of
      TyArrT bnd ret -> case cmpTy tA bnd of
        Just Refl -> Right (Typed ret (TApp fTerm aTerm))
        Nothing   -> Left $
          "internal: application argument type " ++ showTy tA
          ++ " does not match function domain " ++ showTy bnd
      _ -> Left $
        "internal: application of non-function type " ++ showTy tF

elaborateClosed :: AnnTerm -> Either String (Typed (Term ()))
elaborateClosed = elaborate TyNil
