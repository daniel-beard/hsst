module Resolve
  ( resolve
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Syntax
import Diagnostics (Diagnostic(..), mergeSpan)

-- Convert the named-binder surface AST to de Bruijn form.
-- A name resolves to: (1) a binder if found in the local context, else
-- (2) a primitive if its name is in `prims`, else (3) an error pointing at
-- the offending variable's source span.
resolve :: Set Name -> UTerm -> Either Diagnostic IxTerm
resolve prims = go []
  where
    go ctx (UVar sp x) = case lookupCtx x ctx of
      Just i  -> Right (IVar sp i)
      Nothing
        | x `Set.member` prims -> Right (IPrim sp x)
        | otherwise            -> Left Diagnostic
            { diagMessage = "unbound variable: " ++ x
            , diagSpan    = sp
            , diagLabel   = "not found in scope"
            }
    go _   (UStr sp s)  = Right (IStr sp s)
    go _   (UInt sp n)  = Right (IInt sp n)
    go _   (UBool sp b) = Right (IBool sp b)
    go ctx (UApp f a) = do
      f' <- go ctx f
      a' <- go ctx a
      Right (IApp (mergeSpan (ixSpan f') (ixSpan a')) f' a')
    go ctx (ULam x b) = do
      b' <- go (x : ctx) b
      Right (ILam (ixSpan b') b')
    go ctx (ULet x e1 e2) = do
      e1' <- go ctx e1
      e2' <- go (x : ctx) e2
      Right (ILet (mergeSpan (ixSpan e1') (ixSpan e2')) e1' e2')

    lookupCtx :: Name -> [Name] -> Maybe Int
    lookupCtx _ [] = Nothing
    lookupCtx x (y : ys)
      | x == y    = Just 0
      | otherwise = (+ 1) <$> lookupCtx x ys
