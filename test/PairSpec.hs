module PairSpec (spec) where

import Lib (runProgram, runProgramWithLog)
import QQ (hsst)
import Test.Hspec
import TestUtil 

spec :: Spec
spec = do
  describe "pair literals" $ do
    it "prints a pair tab-separated" $
      [hsst| (1, "a") |] `outputs` "1\ta"

    it "parens around a single expression are still grouping" $
      [hsst| (plus(1, 2)) |] `outputs` "3"

    it "f(a, b) is still curried application, f((a, b)) passes a pair" $ do
      [hsst| plus(1, 2) |] `outputs` "3"
      [hsst| fst((1, 2)) |] `outputs` "1"

    it "nests" $
      [hsst| (1, (2, 3)) & snd & snd |] `outputs` "3"

    it "rejects triples, and suggests nesting instead" $
      runProgram "(1, 2, 3)" "" `shouldBeLeftContaining` "nest them instead"

    it "a list of pairs prints one row per line" $
      [hsst| zip(words("a b"), words("x y")) |] `outputs` "a\tx\nb\ty"

    it "a list inside a pair stays on one row, Show-style" $
      [hsst| (words("a b"), 1) |] `outputs` "[\"a\", \"b\"]\t1"

    it "tee renders a pair Show-style" $
      runProgramWithLog "(1, \"a\") & tee" ""
        `shouldBe` Right ("1\ta", ["(1, \"a\")"])

  describe "lambda pair patterns" $ do
    it "destructures a pair" $
      runProgram "words |> map(\\w -> (w, length(w))) |> map(\\(w, n) -> n)" "hi there"
        `shouldBeRight` "2\n5"

    it "destructures nested pairs" $
      [hsst| (1, (2, 3)) & \(a, (b, c)) -> plus(a, c) |] `outputs` "4"

    it "mixes plain and pattern binders" $
      [hsst| (2, 3) & (\x (a, b) -> plus(x, b))(10) |] `outputs` "13"

  describe "pair combinators" $ do
    it "fst / snd / swap" $ do
      [hsst| (1, 2) & fst    |] `outputs` "1"
      [hsst| (1, 2) & snd    |] `outputs` "2"
      [hsst| (1, "a") & swap |] `outputs` "a\t1"

    it "first / second" $ do
      [hsst| ("ab", 1) & first(uppercase) |] `outputs` "AB\t1"
      [hsst| ("ab", true) & second(not)   |] `outputs` "ab\tfalse"

    it "bimap / both" $ do
      [hsst| ("ab", "xyz") & bimap(uppercase, length) |] `outputs` "AB\t3"
      [hsst| ("ab", "xyz") & both(length)             |] `outputs` "2\t3"

    it "fanout pairs up two views of the same input" $
      runProgram "words |> map(fanout(id, length))" "hi there"
        `shouldBeRight` "hi\t2\nthere\t5"

    it "fanout runs left then right, so group sees match's captures" $
      runProgram [hsst|fanout(match(/(\w+)@/), group(1))|] "bob@x.com"
        `shouldBeRight` "bob@\tbob"

    it "uncurry / curry / pair" $ do
      [hsst| (2, 3) & uncurry(plus) |] `outputs` "5"
      [hsst| curry(fst, 1, 2)       |] `outputs` "1"
      [hsst| pair(1, 2)             |] `outputs` "1\t2"

    it "zip / unzip round-trip" $
      [hsst| zip(words("a b"), words("x y")) & unzip & fst & unwords |]
        `outputs` "a b"

  describe "pair errors" $ do
    it "projecting a non-pair is a type mismatch" $
      runProgram "fst(3)" "" `shouldBeLeftContaining` "cannot unify (t1001, t1002) with Int"

    it "suggests & when a pair is piped with |>" $
      runProgram "(1, 2) |> fst" "" `shouldBeLeftContaining` "did you mean & ?"

    it "rejects a pair with an unprintable component" $
      runProgram "(/a/, 1)" "" `shouldBeLeftContaining` "program output has type (Regex, Int), which can't be printed"
