module Syntax
  ( Name,
    -- Surface AST (parser output, named binders)
    UTerm (..),
    -- de Bruijn AST (everything after Resolve)
    IxTerm (..),
    -- Type-annotated de Bruijn AST (output of Infer, input to Elaborate)
    AnnTerm (..),
    -- Surface types
    UType (..),
    TVar,
    prettyUType,
    prettyUTerm,
    prettyIxTerm,
    -- de Bruijn shift + substitution
    shiftIx,
    substIx,
  )
where

import Diagnostics (Span)

type Name = String

type TVar = Int

-- Monomorphic types - output of inference.
data UType
  = TyVar TVar
  | TyString
  | TyInt
  | TyBool
  | TyList UType
  | TyArr UType UType
  deriving (Eq, Show)

-- Surface AST (output of the parser). Variables are named and carry the
-- source span they were parsed from, so name-resolution can point at them.
data UTerm
  = UVar Span Name
  | ULam Name UTerm
  | UApp UTerm UTerm
  | ULet Name UTerm UTerm
  | UStr String
  | UInt Int
  | UBool Bool
  deriving (Eq, Show)

-- de Bruijn AST. Variables are indices; primitives are referenced by name
-- in a separate, global namespace. ILam and ILet both bind one variable
-- (index 0 in their body), so the body's free indices shift by one.
data IxTerm
  = IVar Int
  | IPrim Name
  | IApp IxTerm IxTerm
  | ILam IxTerm
  | ILet IxTerm IxTerm
  | IStr String
  | IInt Int
  | IBool Bool
  deriving (Eq, Show)

-- Shift all free indices >= cutoff by d.
shiftIx :: Int -> Int -> IxTerm -> IxTerm
shiftIx d = go
  where
    go c t = case t of
      IVar i
        | i >= c -> IVar (i + d)
        | otherwise -> t
      IPrim _ -> t
      IStr _ -> t
      IInt _ -> t
      IBool _ -> t
      IApp f a -> IApp (go c f) (go c a)
      ILam b -> ILam (go (c + 1) b)
      ILet r b -> ILet (go c r) (go (c + 1) b)

-- Substitute `s` for index `k` in `t`. Indices > k decrement by 1
-- (because the binder for `k` is being eliminated).
substIx :: Int -> IxTerm -> IxTerm -> IxTerm
substIx k0 s0 = go k0 s0
  where
    go k s t = case t of
      IVar i
        | i == k -> s
        | i > k -> IVar (i - 1)
        | otherwise -> t
      IPrim _ -> t
      IStr _ -> t
      IInt _ -> t
      IBool _ -> t
      IApp f a -> IApp (go k s f) (go k s a)
      ILam b -> ILam (go (k + 1) (shiftIx 1 0 s) b)
      ILet r b -> ILet (go k s r) (go (k + 1) (shiftIx 1 0 s) b)

prettyUType :: UType -> String
prettyUType = go (0 :: Int)
  where
    go _ (TyVar n) = "t" ++ show n
    go _ TyString = "String"
    go _ TyInt = "Int"
    go _ TyBool = "Bool"
    go _ (TyList t) = "[" ++ go 0 t ++ "]"
    go p (TyArr a b) =
      let s = go 1 a ++ " -> " ++ go 0 b
       in if p > 0 then "(" ++ s ++ ")" else s

prettyUTerm :: UTerm -> String
prettyUTerm = go (0 :: Int)
  where
    paren p s = if p > 0 then "(" ++ s ++ ")" else s
    go _ (UVar _ x) = x
    go _ (UStr s) = show s
    go _ (UInt n) = show n
    go _ (UBool b) = if b then "true" else "false"
    go p (ULam x e) = paren p ("\\" ++ x ++ " -> " ++ go 0 e)
    go _ (UApp f a) = go 0 f ++ "(" ++ go 0 a ++ ")"
    go p (ULet x e1 e2) = paren p ("let " ++ x ++ " = " ++ go 0 e1 ++ " in " ++ go 0 e2)

-- AnnTerm: variables, primitives, and lambda binders carry their inferred
-- (post-substitution) type. Application's result type is recoverable from
-- its function child, and literals' types are fixed.
data AnnTerm
  = AVar Int UType
  | APrim Name UType
  | AApp AnnTerm AnnTerm
  | ALam UType AnnTerm -- binder type, body
  | AStr String
  | AInt Int
  | ABool Bool
  deriving (Eq, Show)

prettyIxTerm :: IxTerm -> String
prettyIxTerm = go (0 :: Int)
  where
    paren p s = if p > 0 then "(" ++ s ++ ")" else s
    go _ (IVar i) = "#" ++ show i
    go _ (IPrim n) = n
    go _ (IStr s) = show s
    go _ (IInt n) = show n
    go _ (IBool b) = if b then "true" else "false"
    go p (ILam b) = paren p ("\\. " ++ go 0 b)
    go _ (IApp f a) = go 0 f ++ "(" ++ go 0 a ++ ")"
    go p (ILet r b) = paren p ("let . = " ++ go 0 r ++ " in " ++ go 0 b)
