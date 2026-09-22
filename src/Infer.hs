module Infer
  ( Scheme(..)
  , PrimEnv
  , inferProgram
  ) where

import Control.Monad (foldM)
import Control.Monad.Except (Except, runExcept, throwError, catchError)
import Control.Monad.State.Strict (StateT, evalStateT, get, put, gets, modify)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Data.Set (Set)

import Syntax
import Diagnostics (Span, Diagnostic(..), noSpan)

-- A type scheme: forall vars. ty.
data Scheme = Scheme [TVar] UType
  deriving (Eq, Show)

-- Primitives from name to list of schemes.
-- Single scheme: Ordinary monomorphising
-- Multiple schemes: We figure out overloads below.
type PrimEnv = Map Name [Scheme]

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
      AStr   _ _    -> t
      ARegex _ _    -> t
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

-- Deferred overloads.
-- `ovTy` is a fresh type var - regular inference constrains this.
-- Once substitution is done, pick the scheme that matches best via `resolveOverloads`
data Overload = Overload
  { ovName  :: Name     -- prim name
  , ovTy    :: UType    -- the fresh type var assigned to this occurrence
  , ovSpan  :: Span     -- source span
  , ovCands :: [Scheme] -- the candidate schemes for this name
  }

-- Inference monad: fresh type-var supply, pending overloads, and errors.
data InferS = InferS
  { isNextTy    :: TVar
  , isOverloads :: [Overload]
  }

type Infer a = StateT InferS (Except Diagnostic) a

freshTy :: Infer UType
freshTy = do
  s <- get
  put s { isNextTy = isNextTy s + 1 }
  pure (TyVar (isNextTy s))

recordOverload :: Overload -> Infer ()
recordOverload ov = modify $ \s -> s { isOverloads = ov : isOverloads s }

-- Abort inference with a diagnostic pointing at the given span.
failAt :: Span -> T.Text -> T.Text -> Infer a
failAt sp msg lbl =
  throwError Diagnostic { diagMessage = msg, diagSpan = sp, diagLabel = lbl }

-- Robinson unification. `sp` is the span to blame if the two types don't
-- unify -- threaded down so that the leaf mismatch still points at the
-- expression that caused it (in practice, an application's argument).
unify :: Span -> UType -> UType -> Infer Subst
unify sp a b = case (a, b) of
  (TyChar,   TyChar)   -> pure emptySubst
  (TyStr,    TyStr)    -> pure emptySubst
  (TyInt,    TyInt)    -> pure emptySubst
  (TyBool,   TyBool)   -> pure emptySubst
  (TyRegex,  TyRegex)  -> pure emptySubst
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
         ("type mismatch: cannot unify " <> prettyUType a
            <> " with " <> prettyUType b)
         "mismatched types"

notAFunction :: Span -> UType -> Infer Subst
notAFunction sp t =
  failAt sp ("expected a function, but got " <> prettyUType t) "not a function"

-- A type that definitely isn't a function: a concrete base/list type, 
-- as opposed to an arrow (a function) or a type variable.
notFunctionType :: UType -> Bool
notFunctionType t = case t of
  TyArr _ _ -> False
  TyVar _   -> False
  _         -> True

bindVar :: Span -> TVar -> UType -> Infer Subst
bindVar _ n (TyVar m) | n == m = pure emptySubst
bindVar sp n t
  | n `Set.member` ftvTy t =
      failAt sp
        ("occurs check: " <> prettyUType (TyVar n) <> " in " <> prettyUType t)
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
  IVar   _ _   -> t
  IPrim  _ _   -> t
  IStr   _ _   -> t
  IRegex _ _   -> t
  IChar  _ _   -> t
  IInt   _ _   -> t
  IBool  _ _   -> t

-- Algorithm W over a let-free IxTerm.
-- The local context is a stack of monomorphic types for lambda binders.
-- Returns (subst, type, annotated-term).
infer :: PrimEnv -> [UType] -> IxTerm -> Infer (Subst, UType, AnnTerm)
infer prims ctx e = case e of
  IStr   sp v -> pure (emptySubst, TyStr, AStr sp v)
  IRegex sp v -> pure (emptySubst, TyRegex,  ARegex sp v)
  IChar  sp v -> pure (emptySubst, TyChar,   AChar sp v)
  IInt   sp v -> pure (emptySubst, TyInt,    AInt sp v)
  IBool  sp v -> pure (emptySubst, TyBool,   ABool sp v)

  IVar sp i
    | i < 0 || i >= length ctx ->
        failAt sp ("internal: dangling de Bruijn index " <> T.show i) ""
    | otherwise ->
        let ty = ctx !! i
        in pure (emptySubst, ty, AVar sp i ty)

  IPrim sp x -> case Map.lookup x prims of
    Nothing  -> failAt sp ("unknown primitive: " <> T.pack x) "not a known primitive"
    Just []  -> failAt sp ("unknown primitive: " <> T.pack x) "not a known primitive"
    -- Single scheme: ordinary monomorphising instantiation
    Just [sc] -> do
      ty <- instantiate sc
      pure (emptySubst, ty, APrim sp x ty)
    -- Overloaded prim, assigned a fresh type var.
    -- Scheme choice is deferred until all substitution is done.
    -- Annotated type carries the fresh type var, resolved in `resolveOverloads`
    Just scs -> do
      tv <- freshTy
      recordOverload (Overload x tv sp scs)
      pure (emptySubst, tv, APrim sp x tv)

  ILam sp body -> do
    tv <- freshTy
    (s, tBody, aBody) <- infer prims (tv : ctx) body
    let bt = applyTy s tv
    pure (s, TyArr bt tBody, ALam sp bt aBody)

  IApp sp f a -> do
    tv <- freshTy
    (s1, tF, aF) <- infer prims ctx f
    (s2, tA, aA) <- infer prims (map (applyTy s1) ctx) a
    case (f, applyTy s2 tA) of
      -- `x |> f` desugars to a `|>` application whose left operand x must be a function (|> composes). 
      -- If x is a concrete non-function value, suggest value application: `&`. 
      (IPrim pSpan "|>", tLeft) | notFunctionType tLeft ->
        failAt pSpan
          ("expected a function, but got " <> prettyUType tLeft
             <> "; |> composes functions -- use & to apply a value to a function")
          "did you mean & ?"
      _ -> do
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
  runExcept (evalStateT go (InferS 1000 []))
  where
    go = do
      (s, ty, ann) <- infer prims [] (elimLets t0)
      -- Default to stdin filter type (String)
      -- This allows a polymorphic list prim to run directly on stdin for now.
      -- Otherwise just render as a function.
      sIn <- defaultStdin (applyTy s ty)
      let s1 = sIn `composeS` s
      -- Resolve any outstanding overloads to concrete schemes
      ovs <- gets isOverloads
      s2 <- resolveOverloads s1 ovs
      pure (applyAnn s2 ann)

    defaultStdin ty = case ty of
      TyArr dom _ ->
        unify noSpan dom TyStr `catchError` \_ -> pure emptySubst
      -- Whole program is a bare type var, make it a String filter.
      TyVar _ -> do
        b <- freshTy
        unify noSpan ty (TyArr TyStr b) `catchError` \_ -> pure emptySubst
      _ -> pure emptySubst

-- Resolve deferred overloads
-- Each pass attempts to resolve to a unique, most specific scheme.
-- Substitutions are carried forward, so that an occurence can resolve another in a future pass.
-- Stops when:
-- - Nothing left to resolve
-- - No progress being made (overload is ambiguous)
resolveOverloads :: Subst -> [Overload] -> Infer Subst
resolveOverloads s0 = loop s0
  where
    loop subst pending = do
      (subst', deferred, progressed) <- foldM step (subst, [], False) pending
      case deferred of
        []          -> pure subst'
        (ov : _)
          | progressed -> loop subst' (reverse deferred)
          | otherwise  -> ambiguous ov (applyTy subst' (ovTy ov))

    -- Try to resolve one occurrence under the substitution accumulated so far.
    step (subst, deferred, progressed) ov = do
      let t = applyTy subst (ovTy ov)
      matches <- candidateMatches ov t
      case bestSchemeMatch matches of
        NoMatch        -> noInstance ov t
        UniqueMatch su -> pure (su `composeS` subst, deferred, True)
        -- Defer, might be constrained in a future pass.
        TieMatch       -> pure (subst, ov : deferred, progressed)

    -- Give each scheme fresh vars, then grab each candidate scheme that unifies with this occurrences current type.
    candidateMatches ov t = fmap catMaybes (mapM tryCand (ovCands ov))
      where
        tryCand sc = do
          cand <- instantiate sc
          (do su <- unify (ovSpan ov) cand t
              pure (Just (su, sc)))
            `catchError` \_ -> pure Nothing

    noInstance ov t = failAt (ovSpan ov)
      ("no implementation of " <> T.pack (ovName ov) <> " for type " <> prettyUType t)
      "no matching overload"

    ambiguous ov t = failAt (ovSpan ov)
      ("ambiguous overloaded use of " <> T.pack (ovName ov) <> " at type " <> prettyUType t
        <> " (more than one implementation fits)")
      "ambiguous overload"

-- Matching scheme to candidate scheme. In order of worst to preferred match.
data SchemeMatch = NoMatch | TieMatch | UniqueMatch Subst

-- Prefer most specific (least type variables).
-- E.g. `String -> String` is more specific than `[a] -> [a]`
bestSchemeMatch :: [(Subst, Scheme)] -> SchemeMatch
bestSchemeMatch [] = NoMatch
bestSchemeMatch ms =
  case sortOn vars ms of
    ranked@(best : _) ->
      let minVars = vars best
      in case filter ((== minVars) . vars) ranked of
           [_] -> UniqueMatch (fst best)
           _   -> TieMatch
    [] -> NoMatch
  where
    vars (_, Scheme vs _) = length vs

