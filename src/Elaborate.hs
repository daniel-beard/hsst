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
import Diagnostics (Span, Diagnostic(..))
import Prims 

-- An "internal" error: a soundness bug in an earlier pass, not a user mistake.
-- Still given a span so it lands somewhere useful if it ever fires.
internal :: Span -> String -> Either Diagnostic a
internal sp msg =
  Left Diagnostic { diagMessage = "internal: " ++ msg, diagSpan = sp, diagLabel = "" }

-- Reify a UType at a known span, turning the "ambiguous type" failure into a
-- diagnostic that points at the expression whose type couldn't be pinned down.
reifyAt :: Span -> UType -> Either Diagnostic ExType
reifyAt sp ut = case reifyTy ut of
  Right ex -> Right ex
  Left msg -> Left Diagnostic { diagMessage = msg, diagSpan = sp, diagLabel = "ambiguous type" }

-- The elaboration-time type environment: a stack of GADT singleton types
-- mirroring the de Bruijn lambda-binder stack. Indexed by the same `g`
-- parameter as Term/Var, so the type-equality witness produced for an
-- IVar lookup is exactly the witness Term needs.
data TyEnv g where
  TyNil  :: TyEnv ()
  TyCons :: Ty t -> TyEnv g -> TyEnv (g, t)

-- Look up the i-th binder's GADT type, returning a Typed Var witness.
lookupVar :: Span -> Int -> TyEnv g -> Either Diagnostic (Typed (Var g))
lookupVar sp _ TyNil = internal sp "dangling de Bruijn index in elaboration"
lookupVar _  0 (TyCons ty _)  = Right (Typed ty ZVar)
lookupVar sp n (TyCons _  rest) = do
  Typed ty v <- lookupVar sp (n - 1) rest
  Right (Typed ty (SVar v))

-- Elaborate an annotated, monomorphic term into a Typed (Term g), using
-- the inferred annotations to drive both reifyTy (UType -> Ty) and
-- cmpTy-style equality checks. Each node's span is carried so a failure
-- here points back at the source.
elaborate :: TyEnv g -> AnnTerm -> Either Diagnostic (Typed (Term g))
elaborate env e = case e of
  AStr  _ v -> Right (Typed tyStr   (TStr v))
  AChar _ v -> Right (Typed TyCharT (TChar v))
  AInt  _ v -> Right (Typed TyIntT  (TInt v))
  ABool _ v -> Right (Typed TyBoolT (TBool v))

  AVar sp i ty -> do
    Typed ty' v <- lookupVar sp i env
    -- The annotation should match; cmpTy gives us the runtime witness.
    ExType ety <- reifyAt sp ty
    case cmpTy ety ty' of
      Just Refl -> Right (Typed ty' (TVar v))
      Nothing   -> internal sp $
        "AVar annotation " ++ prettyUType ty
        ++ " disagrees with elaboration env type " ++ showTy ty'

  APrim sp n ty -> do
    ExType rty <- reifyAt sp ty
    case Prims.lookupImpl n rty of
      Just impl -> Right (Typed rty (TPrim n rty impl))
      Nothing   -> Left Diagnostic
        { diagMessage = "primitive " ++ n
                          ++ " has no implementation at type " ++ showTy rty
        , diagSpan    = sp
        , diagLabel   = "unsupported at this type"
        }

  ALam sp binderUTy body -> do
    ExType bTy <- reifyAt sp binderUTy
    Typed retTy body' <- elaborate (TyCons bTy env) body
    Right (Typed (TyArrT bTy retTy) (TLam bTy body'))

  AApp sp f a -> do
    Typed tF fTerm <- elaborate env f
    Typed tA aTerm <- elaborate env a
    case tF of
      TyArrT bnd ret -> case cmpTy tA bnd of
        Just Refl -> Right (Typed ret (TApp fTerm aTerm))
        Nothing   -> internal sp $
          "application argument type " ++ showTy tA
          ++ " does not match function domain " ++ showTy bnd
      _ -> internal sp $
        "application of non-function type " ++ showTy tF

elaborateClosed :: AnnTerm -> Either Diagnostic (Typed (Term ()))
elaborateClosed = elaborate TyNil
