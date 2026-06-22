module RegexCompile
  ( compileRegex
  ) where

import Text.Regex.PCRE (Regex)
import Text.Regex.PCRE.String (compile, compBlank, execBlank)
import System.IO.Unsafe (unsafePerformIO)

-- Compile a raw pattern into a PCRE `Regex`, or report the failure as
-- `(offset, message)` - the library returns us this. 
-- TODO: Don't love the unsafePerformIO here. Re-address later.
{-# NOINLINE compileRegex #-}
compileRegex :: String -> Either (Int, String) Regex
compileRegex pat = unsafePerformIO (compile compBlank execBlank pat)
