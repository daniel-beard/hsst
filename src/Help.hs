module Help
  ( help
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
  | Just ty <- lookup q baseTypes =
      Right (signatures [ (n, sc) | (n, scs) <- Map.toList primSchemes
                                  , sc@(Scheme _ t) <- scs
                                  , ty `mentionedIn` t ])
  | otherwise = Left ("no primitive or type named " <> q)

-- Queryable types
baseTypes :: [(T.Text, UType)]
baseTypes = [ (prettyUType t, t) | t <- [TyChar, TyStr, TyInt, TyBool, TyRegex] ]

mentionedIn :: UType -> UType -> Bool
mentionedIn ty t = ty == t || case t of
  TyList a  -> ty `mentionedIn` a
  TyArr a b -> ty `mentionedIn` a || ty `mentionedIn` b
  _         -> False

signatures :: [(Name, Scheme)] -> T.Text
signatures = T.intercalate "\n" . map (\(n, sc) -> T.pack n <> " :: " <> prettyScheme sc)

-- Quantified vars renamed a, b, c… in order.
prettyScheme :: Scheme -> T.Text
prettyScheme (Scheme vs t) = prettyUTypeWith varName t
  where
    names     = zip vs (map T.singleton ['a'..])
    varName v = fromMaybe ("t" <> T.show v) (lookup v names)
