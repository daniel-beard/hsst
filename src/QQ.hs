{-# LANGUAGE TemplateHaskell #-}

-- | Quasi quoters for hsst source and primitive docs.
module QQ (hsst, doc) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.Text as T
import Control.Monad (when)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Parser (parseProgram)
import PrimDoc (mkPrimDoc)

-- | Raw hsst source as 'T.Text', with no Haskell escaping.
-- Leading and trailing whitespace is trimmed.
--
-- The source is parsed at compile time, so a syntax error is a compile error.
-- Tests that expect a parse error should use a plain string instead.
--
-- > runProgram [hsst| fanout(match(/(\w+)@/), group(1)) |] "bob@x.com"
-- > [hsst| (1, 2) & fst |] `outputs` "1"
hsst :: QuasiQuoter
hsst = expOnly "hsst" $ \s -> do
  let src = trim s
  checkParses src
  [| T.pack src |]

-- | A 'PrimDoc.PrimDoc', written doctest-style:
--
-- > [doc|
-- >   Swaps the order of the two values in a Pair
-- >
-- >   >>> swap(("1", 2))
-- >   (2, "1")
-- > |]
--
-- Text before the first `>>>` is the description.
-- Each `>>>` line is a closed hsst program (no stdin), and the line after it is its Show-style result.
-- Each example is unit tested for correctness.
--
-- Compile errors: an empty description, an example with no result, or an example that doesn't parse.
doc :: QuasiQuoter
doc = expOnly "doc" $ \s -> do
  let (descLines, rest) = break isExample (dedent (lines s))
      summary = trim (unlines (map (dropWhileEnd isSpace) descLines))
  when (null summary) (fail "doc has no description")
  exs <- examples (filter (not . all isSpace) rest)
  mapM_ (checkParses . fst) exs
  [| mkPrimDoc summary exs |]
  where
    isExample = (">>>" `isPrefixOf`) . dropWhile isSpace

    examples [] = pure []
    examples (l : ls)
      | isExample l = case ls of
          r : ls' | not (isExample r) -> ((src, trim r) :) <$> examples ls'
          _ -> fail ("doc example has no result line: " ++ src)
      | otherwise = fail ("doc has text after its examples: " ++ trim l)
      where src = trim (drop 3 (dropWhile isSpace l))

-- Strip the indentation shared by every non-blank line.
dedent :: [String] -> [String]
dedent ls = map (drop n) ls
  where
    n = case [ length (takeWhile isSpace l) | l <- ls, not (all isSpace l) ] of
      [] -> 0
      is -> minimum is

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

expOnly :: String -> (String -> Q Exp) -> QuasiQuoter
expOnly name e = QuasiQuoter
  { quoteExp  = e
  , quotePat  = unsupported "pattern"
  , quoteType = unsupported "type"
  , quoteDec  = unsupported "declaration"
  }
  where
    unsupported :: String -> String -> Q x
    unsupported ctx _ = fail ("the " ++ name ++ " quasi quoter can't be used as a " ++ ctx)

-- GHC already reports where the quote is,
-- so the error is just the parser's message, minus its first line (a placeholder file name and position).
checkParses :: String -> Q ()
checkParses src = case parseProgram src of
  Right _  -> pure ()
  Left err -> fail ("hsst program does not parse:\n" ++ unlines (drop 1 (lines err)))
