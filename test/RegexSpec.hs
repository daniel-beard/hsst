module RegexSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Lib (runProgram)
import Parser
import Syntax

-- parses a regex literal, returning the raw body string.
regexBody :: String -> Either String String
regexBody src = case parseProgram src of
  Right (URegex _ s) -> Right s
  Right other        -> Left ("not a regex literal: " ++ show other)
  Left err           -> Left err

shouldParseTo :: String -> String -> Expectation
shouldParseTo src body = regexBody src `shouldBe` Right body

shouldFail :: String -> Expectation
shouldFail src = regexBody src `shouldSatisfy` either (const True) (const False)

spec :: Spec
spec = do
  describe "regex" $ do
    it "parses a simple pattern" $
      "/.*/" `shouldParseTo` ".*"

    it "keeps repeated quantifiers verbatim" $
      "/.**/" `shouldParseTo` ".**"

    it "allows an empty pattern" $
      "//" `shouldParseTo` ""

    it "keeps an escaped metacharacter verbatim" $
      "/\\./" `shouldParseTo` "\\."

    it "keeps a digit class verbatim" $
      "/\\d+/" `shouldParseTo` "\\d+"

    it "keeps word/space classes verbatim" $
      "/\\w\\s/" `shouldParseTo` "\\w\\s"

    it "keeps a hex escape verbatim" $
      "/\\x41/" `shouldParseTo` "\\x41"

    it "keeps a backreference verbatim" $
      "/(\\w)\\1/" `shouldParseTo` "(\\w)\\1"

    it "does not collapse a double backslash" $
      "/\\\\d/" `shouldParseTo` "\\\\d"

    it "embeds a literal slash via \\/" $
      "/a\\/b/" `shouldParseTo` "a/b"

    it "rejects an unterminated literal" $
      shouldFail "/abc"

  describe "regex compilation" $ do
    it "compiles a valid pattern to a regex value" $
      runProgram "matches(/.*/)" "" `shouldBe` Right "true"

    it "compiles a digit class to a regex value" $
      runProgram "matches(/\\d+/)" "a12" `shouldBe` Right "true"

    it "reports an invalid pattern as a compile error" $
      case runProgram "/[a/" "" of
        Right out -> expectationFailure ("expected a compile error, got: " ++ T.unpack out)
        Left err  -> err `shouldSatisfy` ("invalid regex:" `T.isInfixOf`)

    it "points the caret inside the offending pattern" $
      case runProgram "/[a/" "" of
        Right out -> expectationFailure ("expected a compile error, got: " ++ T.unpack out)
        Left err  -> err `shouldSatisfy` (\e -> "^" `T.isInfixOf` e && "1:" `T.isInfixOf` e)

  describe "matches" $ do
    it "is true when the pattern is found" $
      runProgram "matches(/l+/)" "hello" `shouldBe` Right "true"

    it "is false when the pattern is absent" $
      runProgram "matches(/z+/)" "hello" `shouldBe` Right "false"

    it "applies a value with `&`" $
      runProgram "\"abc123\" & matches(/[0-9]+/)" "" `shouldBe` Right "true"

    it "works as a stdin filter via partial application" $
      runProgram "matches(/^h/)" "hello" `shouldBe` Right "true"


  describe "match / group" $ do
    it "returns the whole match (group 0) as a stdin filter" $
      runProgram "match(/(o\\s)/)" "hello world" `shouldBe` Right "o "

    it "retrieves a capture group recorded by a previous match" $
      runProgram "match(/(o)(\\s)/) |> group(1)" "hello world" `shouldBe` Right "o"

    it "retrieves the second capture group" $
      runProgram "match(/(o)(\\s)/) |> group(2)" "hello world" `shouldBe` Right " "

    it "yields empty string for an out-of-range group" $
      runProgram "match(/o/) |> group(5)" "hello world" `shouldBe` Right ""

    it "works with dollarRef group shorthand" $
      runProgram "match(/(.)/) |> $1" "hi" `shouldBe` Right "h"

  describe "gsub" $ do
    it "replaces every match" $
      runProgram "gsub(/o/, \"0\")" "hello world" `shouldBe` Right "hell0 w0rld"

    it "replaces matches that sit next to each other" $
      runProgram "gsub(/good/, \"bad\")" "goodgoodgood" `shouldBe` Right "badbadbad"

    it "can delete matches" $
      runProgram "gsub(/a/, \"\")" "aaa" `shouldBe` Right ""

    it "leaves the input alone when nothing matches" $
      runProgram "gsub(/z/, \"!\")" "hello" `shouldBe` Right "hello"

    it "treats the replacement literally" $
      runProgram "gsub(/(a)/, \"\\\\1$1\")" "a" `shouldBe` Right "\\1$1"

    it "ignores empty matches" $
      runProgram "gsub(/x*/, \"-\")" "abc" `shouldBe` Right "abc"

    it "ignores empty matches around multi-byte characters" $
      runProgram "gsub(//, \".\")" "hé" `shouldBe` Right "hé"

    it "ignores empty matches around 4-byte characters" $
      runProgram "gsub(//, \".\")" "a😀b" `shouldBe` Right "a😀b"

    it "replaces non-empty matches of a pattern that can match empty" $
      runProgram "gsub(/x*/, \"-\")" "xxaxb" `shouldBe` Right "-a-b"

    it "replaces non-empty matches after a multi-byte character" $
      runProgram "gsub(/x*/, \"-\")" "éxx😀x" `shouldBe` Right "é-😀-"

    it "inserts at the start of the line for /^/" $
      runProgram "gsub(/^/, \"> \")" "abc" `shouldBe` Right "> abc"

    it "inserts at the start of every line for /^/" $
      runProgram "gsub(/^/, \"> \")" "a\nb" `shouldBe` Right "> a\n> b"

    it "inserts into an empty line for /^/" $
      runProgram "gsub(/^/, \"> \")" "" `shouldBe` Right "> "

    it "does not insert after a trailing newline for /^/" $
      runProgram "gsub(/^/, \"> \")" "a\nb\n" `shouldBe` Right "> a\n> b\n"

    it "appends at the end of the line for /$/" $
      runProgram "gsub(/$/, \";\")" "abc" `shouldBe` Right "abc;"

    it "appends at the end of every line for /$/" $
      runProgram "gsub(/$/, \";\")" "a\n\nb" `shouldBe` Right "a;\n;\nb;"

    it "does not append after a trailing newline for /$/" $
      runProgram "gsub(/$/, \";\")" "a\nb\n" `shouldBe` Right "a;\nb;\n"

    it "appends into an empty line for /$/" $
      runProgram "gsub(/$/, \";\")" "" `shouldBe` Right ";"

    it "keeps empty matches of a $-anchored pattern" $
      runProgram "gsub(/x*$/, \"-\")" "abxx\ncd" `shouldBe` Right "ab-\ncd-"

    it "treats an escaped $ as a literal" $
      runProgram "gsub(/\\$/, \"USD\")" "5$" `shouldBe` Right "5USD"

    it "keeps empty matches of a ^-anchored pattern" $
      runProgram "gsub(/^x*/, \"-\")" "abc\nxxd" `shouldBe` Right "-abc\n-d"

    it "replaces non-ASCII text" $
      runProgram "gsub(/é/, \"e\")" "café" `shouldBe` Right "cafe"

  describe "regex compilation offsets" $
    it "points the caret at the right character after non-ASCII text" $
      case runProgram "/é[a/" "" of
        Right out -> expectationFailure ("expected a compile error, got: " ++ T.unpack out)
        Left err  -> err `shouldSatisfy` ("1:5" `T.isInfixOf`)
