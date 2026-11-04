{-# LANGUAGE TemplateHaskell #-}

-- | Quasi quoters for writing tests.
module QQ (hsst) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Parser (parseProgram)

-- | Raw hsst source as 'T.Text', with no Haskell escaping.
-- Leading and trailing whitespace is trimmed.
--
-- The source is parsed at compile time, so a syntax error is a compile error. 
-- Tests that expect a parse error should use a plain string instead.
--
-- > runProgram [hsst| fanout(match(/(\w+)@/), group(1)) |] "bob@x.com"
-- > [hsst| (1, 2) & fst |] `outputs` "1"
hsst :: QuasiQuoter
hsst = QuasiQuoter
  { quoteExp  = \s -> do
      let src = trim s
      checkParses src
      [| T.pack src |]
  , quotePat  = unsupported "pattern"
  , quoteType = unsupported "type"
  , quoteDec  = unsupported "declaration"
  }
  where
    unsupported ctx _ = fail ("the hsst quasi quoter can't be used as a " ++ ctx)
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- GHC already reports where the quote is, 
-- so the error is just the parser's message, minus its first line (a placeholder file name and position).
checkParses :: String -> Q ()
checkParses src = case parseProgram src of
  Right _  -> pure ()
  Left err -> fail ("hsst program does not parse:\n" ++ unlines (drop 1 (lines err)))
