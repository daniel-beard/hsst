{-# LANGUAGE GADTs #-}

module Eval
  ( Env(..)
  , eval
  , render
  ) where

import Data.List (intercalate)

import Core

-- A runtime environment whose shape mirrors the type-level binder stack `g`.
-- Each cons holds a Haskell value of the lambda binder's type.
data Env g where
  ENil  :: Env ()
  ECons :: t -> Env g -> Env (g, t)

lookupEnv :: Var g t -> Env g -> t
lookupEnv ZVar     (ECons x _)   = x
lookupEnv (SVar v) (ECons _ xs)  = lookupEnv v xs

-- The interpreter. Total because the GADT only admits well-typed terms.
eval :: Env g -> Term g t -> t
eval env e = case e of
  TVar v        -> lookupEnv v env
  TStr s        -> s
  TInt n        -> n
  TBool b       -> b
  TLam _ body   -> \x -> eval (ECons x env) body
  TApp f a      -> eval env f (eval env a)
  TPrim _ _ x   -> x

-- Show-style rendering: strings quoted, lists as ["a","b",...], plain
-- otherwise. Function values (the result of a partial pipeline that wasn't
-- applied to anything) get a placeholder.
render :: Ty t -> t -> String
render ty x = case ty of
  TyStr        -> show x
  TyIntT       -> show x
  TyBoolT      -> if x then "true" else "false"
  TyListT a    -> "[" ++ intercalate "," (map (render a) x) ++ "]"
  TyArrT _ _   -> "<function : " ++ showTy ty ++ ">"
