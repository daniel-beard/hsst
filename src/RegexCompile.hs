module RegexCompile
  ( compileRegex
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text.ICU (Regex, regex')
import Data.Text.ICU.Error (ParseError (..), errorName)

-- Compile a raw pattern into an ICU `Regex`, or report the failure as
-- `(offset, message)`, where offset is a character index into the pattern.
--
-- ICU rejects an empty pattern, so `//` compiles as the equivalent `(?:)`.
compileRegex :: String -> Either (Int, String) Regex
compileRegex pat = case regex' [] (T.pack (if null pat then "(?:)" else pat)) of
  Right rx -> Right rx
  Left err -> Left (Maybe.fromMaybe 0 (errOffset err), errorName (errError err))
