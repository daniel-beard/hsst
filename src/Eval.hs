module Eval
  ( Env(..)
  , eval
  , render
  ) where

import Data.List (intercalate)
import Core
import Interp (Interp)

-- A runtime environment whose shape mirrors the type-level binder stack `g`.
-- Each cons holds a runtime value of the lambda binder's type.
data Env g where
  ENil  :: Env ()
  ECons :: t -> Env g -> Env (g, t)

lookupEnv :: Var g t -> Env g -> t
lookupEnv ZVar     (ECons x _)   = x
lookupEnv (SVar v) (ECons _ xs)  = lookupEnv v xs

-- The interpreter. 
-- Applications sequence effects:
-- eval the function, eval the argument, then run the kleisli function.
-- Imposes left-to-right order.
eval :: Env g -> Term g t -> Interp t
eval env e = case e of
  TVar v        -> pure (lookupEnv v env)
  TStr s        -> pure s
  TRegex r      -> pure r
  TChar c       -> pure c
  TInt n        -> pure n
  TBool b       -> pure b
  TLam _ body   -> pure (\x -> eval (ECons x env) body)
  TApp f a      -> do
    f' <- eval env f
    a' <- eval env a
    f' a'
  TPrim _ _ x   -> pure x

-- Show-style rendering: strings quoted, lists as ["a","b",...], plain
-- otherwise. Function values (the result of a partial pipeline that wasn't
-- applied to anything) get a placeholder.
render :: Ty t -> t -> String
render ty x = case ty of
  TyCharT          -> show x
  TyIntT           -> show x
  TyBoolT          -> if x then "true" else "false"
  --TODO: Better render representation?
  TyRegexT         -> "<regex>"
  TyListT TyCharT  -> show x
  TyListT a        -> "[" ++ intercalate "," (map (render a) x) ++ "]"
  TyArrT _ _       -> "<function : " ++ showTy ty ++ ">"
