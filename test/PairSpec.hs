module PairSpec (spec) where

import Lib (runProgram, runProgramWithLog)
import Test.Hspec
import TestUtil (shouldBeLeftContaining)

spec :: Spec
spec = do
  describe "pair literals" $ do
    it "prints a pair tab-separated" $
      runProgram "(1, \"a\")" "" `shouldBe` Right "1\ta"

    it "parens around a single expression are still grouping" $
      runProgram "(plus(1, 2))" "" `shouldBe` Right "3"

    it "f(a, b) is still curried application, f((a, b)) passes a pair" $ do
      runProgram "plus(1, 2)" "" `shouldBe` Right "3"
      runProgram "fst((1, 2))" "" `shouldBe` Right "1"

    it "nests" $
      runProgram "(1, (2, 3)) & snd & snd" "" `shouldBe` Right "3"

    it "rejects triples, and suggests nesting instead" $
      runProgram "(1, 2, 3)" "" `shouldBeLeftContaining` "nest them instead"

    it "a list of pairs prints one row per line" $
      runProgram "zip(words(\"a b\"), words(\"x y\"))" "" `shouldBe` Right "a\tx\nb\ty"

    it "a list inside a pair stays on one row, Show-style" $
      runProgram "(words(\"a b\"), 1)" "" `shouldBe` Right "[\"a\",\"b\"]\t1"

    it "tee renders a pair Show-style" $
      runProgramWithLog "(1, \"a\") & tee" ""
        `shouldBe` Right ("1\ta", ["(1,\"a\")"])

  describe "lambda pair patterns" $ do
    it "destructures a pair" $
      runProgram "words |> map(\\w -> (w, length(w))) |> map(\\(w, n) -> n)" "hi there"
        `shouldBe` Right "2\n5"

    it "destructures nested pairs" $
      runProgram "(1, (2, 3)) & \\(a, (b, c)) -> plus(a, c)" "" `shouldBe` Right "4"

    it "mixes plain and pattern binders" $
      runProgram "(2, 3) & (\\x (a, b) -> plus(x, b))(10)" "" `shouldBe` Right "13"

  describe "pair combinators" $ do
    it "fst / snd / swap" $ do
      runProgram "(1, 2) & fst" "" `shouldBe` Right "1"
      runProgram "(1, 2) & snd" "" `shouldBe` Right "2"
      runProgram "(1, \"a\") & swap" "" `shouldBe` Right "a\t1"

    it "first / second" $ do
      runProgram "(\"ab\", 1) & first(uppercase)" "" `shouldBe` Right "AB\t1"
      runProgram "(\"ab\", true) & second(not)" "" `shouldBe` Right "ab\tfalse"

    it "bimap / both" $ do
      runProgram "(\"ab\", \"xyz\") & bimap(uppercase, length)" "" `shouldBe` Right "AB\t3"
      runProgram "(\"ab\", \"xyz\") & both(length)" "" `shouldBe` Right "2\t3"

    it "fanout pairs up two views of the same input" $
      runProgram "words |> map(fanout(id, length))" "hi there"
        `shouldBe` Right "hi\t2\nthere\t5"

    it "fanout runs left then right, so group sees match's captures" $
      runProgram "fanout(match(/(\\w+)@/), group(1))" "bob@x.com"
        `shouldBe` Right "bob@\tbob"

    it "uncurry / curry / pair" $ do
      runProgram "(2, 3) & uncurry(plus)" "" `shouldBe` Right "5"
      runProgram "curry(fst, 1, 2)" "" `shouldBe` Right "1"
      runProgram "pair(1, 2)" "" `shouldBe` Right "1\t2"

    it "zip / unzip round-trip" $
      runProgram "zip(words(\"a b\"), words(\"x y\")) & unzip & fst & unwords" ""
        `shouldBe` Right "a b"

  describe "pair errors" $ do
    it "projecting a non-pair is a type mismatch" $
      runProgram "fst(3)" "" `shouldBeLeftContaining` "cannot unify (t1001, t1002) with Int"

    it "suggests & when a pair is piped with |>" $
      runProgram "(1, 2) |> fst" "" `shouldBeLeftContaining` "did you mean & ?"

    it "rejects a pair with an unprintable component" $
      runProgram "(/a/, 1)" "" `shouldBeLeftContaining` "program output has type (Regex, Int), which can't be printed"
