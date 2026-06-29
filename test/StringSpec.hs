module StringSpec (spec) where

import Lib (runProgram)
import Test.Hspec

spec :: Spec
spec = describe "strings & chars" $ do
  describe "strings" $ do
    it "uppercase scalar" $
      runProgram "uppercase" "abc"
        `shouldBe` Right "\"ABC\""

    it "base64 round-trip" $
      runProgram "base64 |> unbase64" "haskell"
        `shouldBe` Right "\"haskell\""

    it "base64 encodes non-ASCII as UTF-8" $
      runProgram "base64" "café"
        `shouldBe` Right "\"Y2Fmw6k=\""

    it "base64 round-trips non-ASCII" $
      runProgram "base64 |> unbase64" "café ☕ 你好"
        `shouldBe` Right (show ("café ☕ 你好" :: String))

    it "list string reverse on stdin" $
      runProgram "reverse" "abc"
        `shouldBe` Right "\"cba\""

    it "list string length on stdin" $
      runProgram "length" "héllo"
        `shouldBe` Right "5"

    it "take works on a string" $
      runProgram "take(2)" "abcdef"
        `shouldBe` Right "\"ab\""

    it "map a char op over a string" $
      runProgram "map(upcaseChar)" "abc"
        `shouldBe` Right "\"ABC\""

    it "a top-level function defaults its input to String (stdin filter)" $
      runProgram "\\x -> x" "passthrough"
        `shouldBe` Right "\"passthrough\""

  describe "chars" $ do
    it "char literals have type Char and render quoted" $
      runProgram "upcaseChar('a')" ""
        `shouldBe` Right "'A'"

    it "& feeds a char value into a function" $
      runProgram "'a' & upcaseChar" ""
        `shouldBe` Right "'A'"

    it "charName outputs the correct name for ascii" $
      runProgram "charName('a')" ""
        `shouldBe` Right "\"LATIN SMALL LETTER A\""

    it "codePoint outputs values in U+{hex} format" $
      runProgram "codePoint('a')" ""
        `shouldBe` Right "\"U+0061\""
