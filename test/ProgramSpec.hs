module ProgramSpec (spec) where

import qualified Data.Text as T
import Lib (runProgram)
import Test.Hspec

spec :: Spec
spec = do
  describe "runProgram" $ do
    it "headline" $
      runProgram "words |> map(uppercase) |> take(2) |> map(base64) |> map(unbase64)" "hello world"
        `shouldBe` Right "[\"HELLO\",\"WORLD\"]"

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

    it "& applies a value to a function (left-to-right)" $
      runProgram "3 & plus(2)" ""
        `shouldBe` Right "5"

    it "plus adds two ints" $
      runProgram "plus(2, 3)" ""
        `shouldBe` Right "5"

    it "minus subtracts two ints" $
      runProgram "minus(10, 4)" ""
        `shouldBe` Right "6"

  describe "ad-hoc polymorphism" $ do
    it "String overload is grapheme-aware" $
      -- there are two reverse implementations, `String -> String` and `[a] -> [a]`.
      -- we should always be picking the more specific (less free type vars) option. 
      -- "café" - The 'e' here is 'e' + U+0301 (multi-code-point grapheme cluster.)
      -- Grapheme reverse keeps "e\769" together, where an element-wise reverse would come out invalid `"\769efac"`
      runProgram "reverse" "cafe\769"
        `shouldBe` Right (T.show ("e\769fac" :: T.Text))

    it "picks the [a] -> [a] overload when piped a list of strings" $
      runProgram "words |> reverse" "a b c"
        `shouldBe` Right "[\"c\",\"b\",\"a\"]"

    it "reports no matching overload when no implementation fits the type" $
      runProgram "reverse(42)" ""
        `shouldBe` Left
          --TODO: I need better way not to have to include the unmatched type-var in these...
          (T.unlines
             [ "error: no implementation of reverse for type Int -> String -> t1002"
             , " --> <arg>:1:1"
             , "  |"
             , "1 | reverse(42)"
             , "  | ^^^^^^^ no matching overload"
             ])

  describe "diagnostics" $ do
    it "renders an unbound variable with a rustc-like error format" $
      runProgram "words |> foo" ""
        `shouldBe` Left
          (T.unlines
             [ "error: unbound variable: foo"
             , " --> <arg>:1:10"
             , "  |"
             , "1 | words |> foo"
             , "  |          ^^^ not found in scope"
             ])

    it "points a type mismatch (from inference) at the offending argument" $
      runProgram "plus(2, \"x\")" ""
        `shouldBe` Left
          (T.unlines
             [ "error: type mismatch: cannot unify Int with String"
             , " --> <arg>:1:9"
             , "  |"
             , "1 | plus(2, \"x\")"
             , "  |         ^^^ mismatched types"
             ])

    it "suggests & when the left operand of |> is a value" $
      -- `|>` is composition, so its left side must be a function; a value there
      -- is almost certainly meant as `&` (value application).
      runProgram "'c' |> upcaseChar" ""
        `shouldBe` Left
          (T.unlines
             [ "error: expected a function, but got Char; |> composes functions -- use & to apply a value to a function"
             , " --> <arg>:1:5"
             , "  |"
             , "1 | 'c' |> upcaseChar"
             , "  |     ^^ did you mean & ?"
             ])

    it "reports a plain not-a-function error away from |> (no & hint)" $
      runProgram "compose('c', upcaseChar)" ""
        `shouldBe` Left
          (T.unlines
             [ "error: expected a function, but got Char"
             , " --> <arg>:1:9"
             , "  |"
             , "1 | compose('c', upcaseChar)"
             , "  |         ^^^ not a function"
             ])

    it "points an ambiguous top-level type (from elaboration) at the expression" $
      -- `compose` expects a function as input, so a program that is only compose is ambiguous
      runProgram "compose" ""
        `shouldBe` Left
          (T.unlines
             [ "error: ambiguous type: free type variable t1000 survived inference (the program is polymorphic at the top level and would need an annotation to run)"
             , " --> <arg>:1:1"
             , "  |"
             , "1 | compose"
             , "  | ^^^^^^^ ambiguous type"
             ])
