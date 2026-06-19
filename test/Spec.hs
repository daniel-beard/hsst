module Main (main) where

import Lib (runProgram)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "runProgram" $ do
    it "headline" $
      runProgram "words |> map(uppercase) |> take(2) |> map(base64) |> map(unbase64)" "hello world"
        `shouldBe` Right "[\"HELLO\",\"WORLD\"]"

    it "uppercase scalar" $
      runProgram "uppercase" "abc"
        `shouldBe` Right "\"ABC\""

    it "base64 round-trip" $
      runProgram "base64 |> unbase64" "haskell"
        `shouldBe` Right "\"haskell\""

    it "let-polymorphism" $
      -- id is used at type (String -> String) once and at type
      -- ([String] -> [String]) once, in the same program.
      runProgram "let id = \\x -> x in id |> words |> id |> length" "one two three"
        `shouldBe` Right "3"

    it "nested lets correctly use de Bruijn indices" $
      runProgram "let a = \\x -> x in a |> words |> let a = reverse in a |> unwords" "a b"
        `shouldBe` Right "\"b a\""

    it "lambda inside map" $
      runProgram "words |> map(\\s -> uppercase(s))" "ab cd"
        `shouldBe` Right "[\"AB\",\"CD\"]"

    it "literal program (no stdin function)" $
      runProgram "42" ""
        `shouldBe` Right "42"

    it "compose (|>) is left-to-right" $
      runProgram "lines |> length" "a\nb\nc\n"
        `shouldBe` Right "3"

    it "plus adds two ints" $
      runProgram "plus(2, 3)" ""
        `shouldBe` Right "5"

    it "minus subtracts two ints" $
      runProgram "minus(10, 4)" ""
        `shouldBe` Right "6"
