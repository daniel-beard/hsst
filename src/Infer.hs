module Infer
  ( Scheme(..)
  , PrimEnv
  , inferProgram
  ) where

import Control.Monad.Except (Except, runExcept, throwError, catchError)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import Data.Set (Set)

import Syntax
import Diagnostics (Span, Diagnostic(..), noSpan)

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
      AVar  sp i ty -> AVar sp i (applyTy s ty)
      APrim sp n ty -> APrim sp n (applyTy s ty)
      AApp  sp f a  -> AApp sp (go f) (go a)
      ALam  sp bt b -> ALam sp (applyTy s bt) (go b)
      AStr  _ _     -> t
      AChar _ _     -> t
      AInt  _ _     -> t
      ABool _ _     -> t

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

type Infer a = StateT Fresh (Except Diagnostic) a

freshTy :: Infer UType
freshTy = do
  Fresh n <- get
  put (Fresh (n + 1))
  pure (TyVar n)

-- Abort inference with a diagnostic pointing at the given span.
failAt :: Span -> String -> String -> Infer a
failAt sp msg lbl =
  throwError Diagnostic { diagMessage = msg, diagSpan = sp, diagLabel = lbl }

-- Robinson unification. `sp` is the span to blame if the two types don't
-- unify -- threaded down so that the leaf mismatch still points at the
-- expression that caused it (in practice, an application's argument).
unify :: Span -> UType -> UType -> Infer Subst
unify sp a b = case (a, b) of
  (TyChar,   TyChar)   -> pure emptySubst
  (TyInt,    TyInt)    -> pure emptySubst
  (TyBool,   TyBool)   -> pure emptySubst
  (TyList x, TyList y) -> unify sp x y
  (TyArr l1 r1, TyArr l2 r2) -> do
    s1 <- unify sp l1 l2
    s2 <- unify sp (applyTy s1 r1) (applyTy s1 r2)
    pure (s2 `composeS` s1)
  (TyVar n, t) -> bindVar sp n t
  (t, TyVar n) -> bindVar sp n t
  -- A function was required but the other side is a concrete non-function
  -- e.g. `'c' |> upcaseChar`, since |> is composition and wants functions on both sides.
  (TyArr _ _, _) -> notAFunction sp b
  (_, TyArr _ _) -> notAFunction sp a
  _ -> failAt sp
         ("type mismatch: cannot unify " ++ prettyUType a
            ++ " with " ++ prettyUType b)
         "mismatched types"

notAFunction :: Span -> UType -> Infer Subst
notAFunction sp t =
  failAt sp ("expected a function, but got " ++ prettyUType t) "not a function"

bindVar :: Span -> TVar -> UType -> Infer Subst
bindVar _ n (TyVar m) | n == m = pure emptySubst
bindVar sp n t
  | n `Set.member` ftvTy t =
      failAt sp
        ("occurs check: " ++ prettyUType (TyVar n) ++ " in " ++ prettyUType t)
        "infinite type"
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
  ILet _ e1 e2 -> elimLets (substIx 0 (elimLets e1) e2)
  IApp sp f a  -> IApp sp (elimLets f) (elimLets a)
  ILam sp b    -> ILam sp (elimLets b)
  IVar  _ _    -> t
  IPrim _ _    -> t
  IStr  _ _    -> t
  IChar _ _    -> t
  IInt  _ _    -> t
  IBool _ _    -> t

-- Algorithm W over a let-free IxTerm.
-- The local context is a stack of monomorphic types for lambda binders.
-- Returns (subst, type, annotated-term).
infer :: PrimEnv -> [UType] -> IxTerm -> Infer (Subst, UType, AnnTerm)
infer prims ctx e = case e of
  IStr  sp v -> pure (emptySubst, TyList TyChar, AStr sp v)
  IChar sp v -> pure (emptySubst, TyChar,   AChar sp v)
  IInt  sp v -> pure (emptySubst, TyInt,    AInt sp v)
  IBool sp v -> pure (emptySubst, TyBool,   ABool sp v)

  IVar sp i
    | i < 0 || i >= length ctx ->
        failAt sp ("internal: dangling de Bruijn index " ++ show i) ""
    | otherwise ->
        let ty = ctx !! i
        in pure (emptySubst, ty, AVar sp i ty)

  IPrim sp x -> case Map.lookup x prims of
    Nothing -> failAt sp ("unknown primitive: " ++ x) "not a known primitive"
    Just sc -> do
      ty <- instantiate sc
      pure (emptySubst, ty, APrim sp x ty)

  ILam sp body -> do
    tv <- freshTy
    (s, tBody, aBody) <- infer prims (tv : ctx) body
    let bt = applyTy s tv
    pure (s, TyArr bt tBody, ALam sp bt aBody)

  IApp sp f a -> do
    tv <- freshTy
    (s1, tF, aF) <- infer prims ctx f
    (s2, tA, aA) <- infer prims (map (applyTy s1) ctx) a
    -- Blame the argument: that's the expression whose type has to fit the
    -- function's domain, and the one a "wrong type" message is about.
    s3 <- unify (ixSpan a) (applyTy s2 tF) (TyArr tA tv)
    let s = s3 `composeS` s2 `composeS` s1
    pure (s, applyTy s3 tv, AApp sp aF aA)

  ILet{} ->
    failAt noSpan
      "internal: ILet should have been eliminated before inference" ""

inferProgram :: PrimEnv -> IxTerm -> Either Diagnostic AnnTerm
inferProgram prims t0 =
  runExcept (evalStateT go (Fresh 1000))
  where
    go = do
      (s, ty, ann) <- infer prims [] (elimLets t0)
      -- The program is run as a filter over stdin, which is always a String.
      -- So if the whole program is a function whose input is still
      -- unconstrained, default that input to String. This lets a bare
      -- polymorphic list op (reverse, length, take, ...) run directly on
      -- stdin. If the input cannot be a String (e.g. Int -> Int), leave it:
      -- the value renders as a function instead.
      sIn <- defaultStdin (applyTy s ty)
      pure (applyAnn (sIn `composeS` s) ann)

    defaultStdin ty = case ty of
      TyArr dom _ ->
        unify noSpan dom (TyList TyChar) `catchError` \_ -> pure emptySubst
      _ -> pure emptySubst
