module RegexSpec (spec) where

import Data.List (isInfixOf)
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

  --TODO: Probably want a better representation than `<regex>` here. Address later.
  describe "regex compilation" $ do
    it "compiles a valid pattern to a regex value" $
      runProgram "/.*/" "" `shouldBe` Right "<regex>"

    it "compiles a digit class to a regex value" $
      runProgram "/\\d+/" "" `shouldBe` Right "<regex>"

    it "reports an invalid pattern as a compile error" $
      case runProgram "/[a/" "" of
        Right out -> expectationFailure ("expected a compile error, got: " ++ out)
        Left err  -> err `shouldSatisfy` ("invalid regex:" `isInfixOf`)

    it "points the caret inside the offending pattern" $
      case runProgram "/[a/" "" of
        Right out -> expectationFailure ("expected a compile error, got: " ++ out)
        Left err  -> err `shouldSatisfy` (\e -> "^" `isInfixOf` e && "1:" `isInfixOf` e)

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
      runProgram "match(/(o\\s)/)" "hello world" `shouldBe` Right "\"o \""

    it "retrieves a capture group recorded by a previous match" $
      runProgram "match(/(o)(\\s)/) |> group(1)" "hello world" `shouldBe` Right "\"o\""

    it "retrieves the second capture group" $
      runProgram "match(/(o)(\\s)/) |> group(2)" "hello world" `shouldBe` Right "\" \""

    it "yields empty string for an out-of-range group" $
      runProgram "match(/o/) |> group(5)" "hello world" `shouldBe` Right "\"\""
