module Resolve
  ( resolve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Syntax
import Diagnostics (Diagnostic(..))

-- Convert the named-binder surface AST to de Bruijn form.
-- A name resolves to: (1) a binder if found in the local context, else
-- (2) a primitive if its name is in `prims`, else (3) an error pointing at
-- the offending variable's source span.
resolve :: Set Name -> UTerm -> Either Diagnostic IxTerm
resolve prims = go []
  where
    go ctx (UVar sp x) = case lookupCtx x ctx of
      Just i  -> Right (IVar i)
      Nothing
        | x `Set.member` prims -> Right (IPrim x)
        | otherwise            -> Left Diagnostic
            { diagMessage = "unbound variable: " ++ x
            , diagSpan    = sp
            , diagLabel   = "not found in scope"
            }
    go _   (UStr s)  = Right (IStr s)
    go _   (UInt n)  = Right (IInt n)
    go _   (UBool b) = Right (IBool b)
    go ctx (UApp f a) = IApp <$> go ctx f <*> go ctx a
    go ctx (ULam x b) = ILam <$> go (x : ctx) b
    go ctx (ULet x e1 e2) = ILet <$> go ctx e1 <*> go (x : ctx) e2

    lookupCtx :: Name -> [Name] -> Maybe Int
    lookupCtx _ [] = Nothing
    lookupCtx x (y : ys)
      | x == y    = Just 0
      | otherwise = (+ 1) <$> lookupCtx x ys
