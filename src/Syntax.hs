module Syntax
  ( Name,
    -- Surface AST (parser output, named binders)
    UTerm (..),
    -- de Bruijn AST (everything after Resolve)
    IxTerm (..),
    ixSpan,
    -- Type-annotated de Bruijn AST (output of Infer, input to Elaborate)
    AnnTerm (..),
    annSpan,
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

import Diagnostics (Span(..))

type Name = String

type TVar = Int

-- Monomorphic types - output of inference.
-- Strings are not a distinct type: a String is a list of Char (TyList TyChar),
data UType
  = TyVar TVar
  | TyChar
  | TyInt
  | TyBool
  | TyRegex
  | TyList UType
  | TyArr UType UType
  deriving (Eq, Show)

-- Surface AST (output of the parser). Variables and literals carry the
-- source span they were parsed from, so later passes can point at them.
-- ULam/UApp/ULet are composite: their span is recovered from their children
-- during resolution rather than stored here.
data UTerm
  = UVar  Span  Name
  | ULam  Name  UTerm
  | UApp  UTerm UTerm
  | ULet  Name  UTerm  UTerm
  | UStr   Span  String
  | URegex Span  String -- raw pattern text
  | UChar  Span  Char
  | UInt   Span  Int
  | UBool  Span  Bool
  deriving (Eq, Show)

-- de Bruijn AST. Variables are indices; primitives are referenced by name
-- in a separate, global namespace. ILam and ILet both bind one variable
-- (index 0 in their body), so the body's free indices shift by one.
-- Every node carries the source span it came from, so that a type error
-- discovered after resolution can still point at the offending text.
data IxTerm
  = IVar  Span Int
  | IPrim Span Name
  | IApp  Span IxTerm IxTerm
  | ILam  Span IxTerm
  | ILet  Span IxTerm IxTerm
  | IStr   Span String
  | IRegex Span String -- raw pattern text, compiled later. See `Elaborate.hs`
  | IChar  Span Char
  | IInt   Span Int
  | IBool  Span Bool
  deriving (Eq, Show)

-- The span an IxTerm was resolved from.
ixSpan :: IxTerm -> Span
ixSpan t = case t of
  IVar   sp _   -> sp
  IPrim  sp _   -> sp
  IApp   sp _ _ -> sp
  ILam   sp _   -> sp
  ILet   sp _ _ -> sp
  IStr   sp _   -> sp
  IRegex sp _   -> sp
  IChar  sp _   -> sp
  IInt   sp _   -> sp
  IBool  sp _   -> sp

-- Shift all free indices >= cutoff by d.
shiftIx :: Int -> Int -> IxTerm -> IxTerm
shiftIx d = go
  where
    go c t = case t of
      IVar sp i
        | i >= c -> IVar sp (i + d)
        | otherwise -> t
      IPrim _ _   -> t
      IStr _ _    -> t
      IRegex _ _  -> t
      IChar _ _   -> t
      IInt _ _    -> t
      IBool _ _   -> t
      IApp sp f a -> IApp sp (go c f) (go c a)
      ILam sp b   -> ILam sp (go (c + 1) b)
      ILet sp r b -> ILet sp (go c r) (go (c + 1) b)

-- Substitute `s` for index `k` in `t`. Indices > k decrement by 1
-- (because the binder for `k` is being eliminated). The substituted copy of
-- `s` keeps its own (definition-site) spans.
substIx :: Int -> IxTerm -> IxTerm -> IxTerm
substIx k0 s0 = go k0 s0
  where
    go k s t = case t of
      IVar _ i
        | i == k -> s
        | i > k -> IVar (ixSpan t) (i - 1)
        | otherwise -> t
      IPrim _ _   -> t
      IStr _ _    -> t
      IRegex _ _  -> t
      IChar _ _   -> t
      IInt _ _    -> t
      IBool _ _   -> t
      IApp sp f a -> IApp sp (go k s f) (go k s a)
      ILam sp b   -> ILam sp (go (k + 1) (shiftIx 1 0 s) b)
      ILet sp r b -> ILet sp (go k s r) (go (k + 1) (shiftIx 1 0 s) b)

prettyUType :: UType -> String
prettyUType = go (0 :: Int)
  where
    go _ (TyVar n)       = "t" ++ show n
    go _ TyChar          = "Char"
    go _ TyInt           = "Int"
    go _ TyBool          = "Bool"
    go _ TyRegex         = "Regex"
    go _ (TyList TyChar) = "String"
    go _ (TyList t)      = "[" ++ go 0 t ++ "]"
    go p (TyArr a b) =
      let s = go 1 a ++ " -> " ++ go 0 b
       in if p > 0 then "(" ++ s ++ ")" else s

prettyUTerm :: UTerm -> String
prettyUTerm = go (0 :: Int)
  where
    paren p s           = if p > 0 then "(" ++ s ++ ")" else s
    go _ (UVar _ x)     = x
    go _ (UStr _ s)     = show s
    go _ (URegex _ s)   = "/" ++ s ++ "/"
    go _ (UChar _ c)    = show c
    go _ (UInt _ n)     = show n
    go _ (UBool _ b)    = if b then "true" else "false"
    go p (ULam x e)     = paren p ("\\" ++ x ++ " -> " ++ go 0 e)
    go _ (UApp f a)     = go 0 f ++ "(" ++ go 0 a ++ ")"
    go p (ULet x e1 e2) = paren p ("let " ++ x ++ " = " ++ go 0 e1 ++ " in " ++ go 0 e2)

-- AnnTerm: variables, primitives, and lambda binders carry their inferred
-- (post-substitution) type. Application's result type is recoverable from
-- its function child, and literals' types are fixed. Every node also carries
-- the source span threaded through from the IxTerm it was inferred from, so
-- elaboration errors can point at the source.
data AnnTerm
  = AVar  Span Int     UType
  | APrim Span Name    UType
  | AApp   Span AnnTerm AnnTerm
  | ALam   Span UType   AnnTerm -- binder type, body
  | AStr   Span String
  | ARegex Span String -- raw pattern text
  | AChar  Span Char
  | AInt   Span Int
  | ABool  Span Bool
  deriving (Eq, Show)

-- The span an AnnTerm was inferred from.
annSpan :: AnnTerm -> Span
annSpan t = case t of
  AVar   sp _ _ -> sp
  APrim  sp _ _ -> sp
  AApp   sp _ _ -> sp
  ALam   sp _ _ -> sp
  AStr   sp _   -> sp
  ARegex sp _   -> sp
  AChar  sp _   -> sp
  AInt   sp _   -> sp
  ABool  sp _   -> sp

prettyIxTerm :: IxTerm -> String
prettyIxTerm = go (0 :: Int)
  where
    paren p s         = if p > 0 then "(" ++ s ++ ")" else s
    go _ (IVar _ i)    = "#" ++ show i
    go _ (IPrim _ n)   = n
    go _ (IStr _ s)    = show s
    go _ (IRegex _ s)  = "/" ++ s ++ "/"
    go _ (IChar _ c)   = show c
    go _ (IInt _ n)   = show n
    go _ (IBool _ b)  = if b then "true" else "false"
    go p (ILam _ b)   = paren p ("\\. " ++ go 0 b)
    go _ (IApp _ f a) = go 0 f ++ "(" ++ go 0 a ++ ")"
    go p (ILet _ r b) = paren p ("let . = " ++ go 0 r ++ " in " ++ go 0 b)
