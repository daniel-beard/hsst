module Help
  ( help
  , helpTypes
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isNothing)

import Infer (Scheme(..))
import PrimDoc (PrimDoc(..), Example(..))
import Prims (primSchemes, primHelpEntries)
import Syntax

-- `hsst -h <query>`: a primitive name or a type name.
help :: T.Text -> Either T.Text T.Text
help q
  | Just entries <- Map.lookup name primHelpEntries = Right (primitiveHelp name entries)
  | Just isTy    <- lookup q baseTypes              = Right (typeHelp isTy)
  | otherwise                                       = Left ("no primitive or type named " <> q)
  where
    name = T.unpack q

-- Each overload's signature, description and examples.
-- If no overload has docs yet, just the signatures, one per line.
primitiveHelp :: Name -> [(Scheme, Maybe PrimDoc)] -> T.Text
primitiveHelp n entries
  | all (isNothing . snd) entries = signatures (map withName entries)
  | otherwise                     = T.intercalate "\n\n" (map (primHelp n) entries)
  where
    withName (sc, _) = (n, sc)

-- Every primitive signature that mentions the type.
typeHelp :: (UType -> Bool) -> T.Text
typeHelp isTy = signatures
  [ (n, sc)
  | (n, scs)        <- Map.toList primSchemes
  , n `notElem` desugarOnly
  , sc@(Scheme _ t) <- scs
  , isTy `mentionedIn` t
  ]

-- Only emitted by the parser for `(x, y)` literals and `\(x, y) ->` patterns.
-- `pair` and `uncurry` are the callable versions.
desugarOnly :: [Name]
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

-- One overload, as blank-line separated blocks:
--
-- > swap :: (a, b) -> (b, a)
-- >
-- > Swaps the order of the two values in a pair
-- >
-- > >>> swap(("1", 2)) 
-- > (2, "1")
--
-- An undocumented overload is just its signature.
primHelp :: Name -> (Scheme, Maybe PrimDoc) -> T.Text
primHelp n (sc, md) = T.intercalate "\n\n" (signature (n, sc) : maybe [] docBlocks md)

-- The description, then all examples together as one block.
docBlocks :: PrimDoc -> [T.Text]
docBlocks d = case docExamples d of
  []  -> [docSummary d]
  exs -> [docSummary d, T.intercalate "\n" (map example exs)]

example :: Example -> T.Text
example e = ">>> " <> exSource e <> "\n" <> exResult e

signatures :: [(Name, Scheme)] -> T.Text
signatures = T.intercalate "\n" . map signature

signature :: (Name, Scheme) -> T.Text
signature (n, sc) = T.pack n <> " :: " <> prettyScheme sc

-- Quantified vars renamed a, b, c… in order.
prettyScheme :: Scheme -> T.Text
prettyScheme (Scheme vs t) = prettyUTypeWith varName t
  where
    names     = zip vs (map T.singleton ['a'..])
    varName v = fromMaybe ("t" <> T.show v) (lookup v names)
