module Infer
  ( Scheme(..)
  , PrimEnv
  , inferProgram
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.Set (Set)

import Syntax

-- A type scheme: forall vars. ty.
data Scheme = Scheme [TVar] UType
  deriving (Eq, Show)

-- The environment of primitives, keyed by name.
type PrimEnv = Map Name Scheme

-- Substitution from type variables to monomorphic types.
type Subst = Map TVar UType

emptySubst :: Subst
emptySubst = Map.empty

applyTy :: Subst -> UType -> UType
applyTy s t = case t of
  TyVar n   -> Map.findWithDefault t n s
  TyArr a b -> TyArr (applyTy s a) (applyTy s b)
  TyList a  -> TyList (applyTy s a)
  _         -> t

applyAnn :: Subst -> AnnTerm -> AnnTerm
applyAnn s = go
  where
    go t = case t of
      AVar  i ty    -> AVar i (applyTy s ty)
      APrim n ty    -> APrim n (applyTy s ty)
      AApp  f a     -> AApp (go f) (go a)
      ALam  bt b    -> ALam (applyTy s bt) (go b)
      AStr  _       -> t
      AInt  _       -> t
      ABool _       -> t

-- s2 ∘ s1
composeS :: Subst -> Subst -> Subst
composeS s2 s1 = Map.map (applyTy s2) s1 `Map.union` s2

ftvTy :: UType -> Set TVar
ftvTy t = case t of
  TyVar n   -> Set.singleton n
  TyArr a b -> ftvTy a `Set.union` ftvTy b
  TyList a  -> ftvTy a
  _         -> Set.empty

-- Inference monad: fresh type-var supply + errors.
newtype Fresh = Fresh { nextTy :: TVar }

type Infer a = StateT Fresh (Except String) a

freshTy :: Infer UType
freshTy = do
  Fresh n <- get
  put (Fresh (n + 1))
  pure (TyVar n)

inferErr :: String -> Infer a
inferErr = throwError

-- Robinson unification.
unify :: UType -> UType -> Infer Subst
unify a b = case (a, b) of
  (TyString, TyString) -> pure emptySubst
  (TyInt,    TyInt)    -> pure emptySubst
  (TyBool,   TyBool)   -> pure emptySubst
  (TyList x, TyList y) -> unify x y
  (TyArr l1 r1, TyArr l2 r2) -> do
    s1 <- unify l1 l2
    s2 <- unify (applyTy s1 r1) (applyTy s1 r2)
    pure (s2 `composeS` s1)
  (TyVar n, t) -> bindVar n t
  (t, TyVar n) -> bindVar n t
  _ -> inferErr $ "type mismatch: cannot unify "
                ++ prettyUType a ++ " with " ++ prettyUType b

bindVar :: TVar -> UType -> Infer Subst
bindVar n (TyVar m) | n == m = pure emptySubst
bindVar n t
  | n `Set.member` ftvTy t =
      inferErr $ "occurs check: " ++ prettyUType (TyVar n)
              ++ " in " ++ prettyUType t
  | otherwise = pure (Map.singleton n t)

-- Instantiate a scheme by refreshing its bound type variables.
instantiate :: Scheme -> Infer UType
instantiate (Scheme vs t) = do
  vs' <- mapM (const freshTy) vs
  let s = Map.fromList (zip vs vs')
  pure (applyTy s t)

-- Eliminate every ILet by substituting the binding into its body.
-- Each substituted copy is structurally shared but inferred independently
-- by the W pass below — that's how this gets us let-polymorphism.
elimLets :: IxTerm -> IxTerm
elimLets t = case t of
  ILet e1 e2 -> elimLets (substIx 0 (elimLets e1) e2)
  IApp f a   -> IApp (elimLets f) (elimLets a)
  ILam b     -> ILam (elimLets b)
  IVar  _    -> t
  IPrim _    -> t
  IStr  _    -> t
  IInt  _    -> t
  IBool _    -> t

-- Algorithm W over a let-free IxTerm.
-- The local context is a stack of monomorphic types for lambda binders.
-- Returns (subst, type, annotated-term).
infer :: PrimEnv -> [UType] -> IxTerm -> Infer (Subst, UType, AnnTerm)
infer prims ctx e = case e of
  IStr  v -> pure (emptySubst, TyString, AStr v)
  IInt  v -> pure (emptySubst, TyInt,    AInt v)
  IBool v -> pure (emptySubst, TyBool,   ABool v)

  IVar i
    | i < 0 || i >= length ctx ->
        inferErr $ "internal: dangling de Bruijn index " ++ show i
    | otherwise ->
        let ty = ctx !! i
        in pure (emptySubst, ty, AVar i ty)

  IPrim x -> case Map.lookup x prims of
    Nothing -> inferErr $ "unknown primitive: " ++ x
    Just sc -> do
      ty <- instantiate sc
      pure (emptySubst, ty, APrim x ty)

  ILam body -> do
    tv <- freshTy
    (s, tBody, aBody) <- infer prims (tv : ctx) body
    let bt = applyTy s tv
    pure (s, TyArr bt tBody, ALam bt aBody)

  IApp f a -> do
    tv <- freshTy
    (s1, tF, aF) <- infer prims ctx f
    (s2, tA, aA) <- infer prims (map (applyTy s1) ctx) a
    s3 <- unify (applyTy s2 tF) (TyArr tA tv)
    let s = s3 `composeS` s2 `composeS` s1
    pure (s, applyTy s3 tv, AApp aF aA)

  ILet _ _ ->
    inferErr "internal: ILet should have been eliminated before inference"

inferProgram :: PrimEnv -> IxTerm -> Either String AnnTerm
inferProgram prims t0 =
  let t = elimLets t0
  in case runExcept (evalStateT (infer prims [] t) (Fresh 1000)) of
    Left err           -> Left err
    Right (s, _ty, ann) -> Right (applyAnn s ann)
