module Help
  ( help
  , helpTypes
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import Infer (Scheme(..))
import Prims (primSchemes)
import Syntax

-- `hsst -h <query>`:
-- - primitive query: all overloads listed
-- - type name query: every primitive signature containing the type
help :: T.Text -> Either T.Text T.Text
help q
  | Just scs <- Map.lookup (T.unpack q) primSchemes =
      Right (signatures [ (T.unpack q, sc) | sc <- scs ])
  | Just isTy <- lookup q baseTypes =
      Right (signatures [ (n, sc) | (n, scs) <- Map.toList primSchemes
                                  , n `notElem` desugarOnly
                                  , sc@(Scheme _ t) <- scs
                                  , isTy `mentionedIn` t ])
  | otherwise = Left ("no primitive or type named " <> q)
  where
    -- Only emitted by the parser for `(x, y)` literals and `\(x, y) ->` patterns.
    -- `pair` and `uncurry` are the callable versions.
    desugarOnly = ["(,)", "\\(,)"]

helpTypes :: T.Text
helpTypes = ("Available types. Use these with 'hsst -h {TypeName}':\n\n" :: T.Text) <> T.intercalate "\n" (map fst baseTypes)

-- Queryable types, as a predicate on a (sub)type.
-- `Pair` matches any pair, of any type.
baseTypes :: [(T.Text, UType -> Bool)]
baseTypes =
  [ (prettyUType t, (== t)) | t <- [TyChar, TyStr, TyInt, TyBool, TyRegex] ]
    ++ [("Pair", isPair)]
  where
    isPair (TyPair _ _) = True
    isPair _            = False

mentionedIn :: (UType -> Bool) -> UType -> Bool
mentionedIn isTy t = isTy t || case t of
  TyList a   -> isTy `mentionedIn` a
  TyPair a b -> isTy `mentionedIn` a || isTy `mentionedIn` b
  TyArr a b  -> isTy `mentionedIn` a || isTy `mentionedIn` b
  _          -> False

signatures :: [(Name, Scheme)] -> T.Text
signatures = T.intercalate "\n" . map (\(n, sc) -> T.pack n <> " :: " <> prettyScheme sc)

-- Quantified vars renamed a, b, c… in order.
prettyScheme :: Scheme -> T.Text
prettyScheme (Scheme vs t) = prettyUTypeWith varName t
  where
    names     = zip vs (map T.singleton ['a'..])
    varName v = fromMaybe ("t" <> T.show v) (lookup v names)
