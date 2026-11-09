module HelpSpec (spec) where

import qualified Data.Text as T
import Help (help)
import Test.Hspec

spec :: Spec
spec = describe "help" $ do
  it "lists every overload of a primitive, in declaration order" $
    help "uppercase"
      `shouldBe` Right "uppercase :: String -> String\nuppercase :: Char -> Char"

  it "names scheme type variables a, b, c" $
    help "compose"
      `shouldBe` Right "compose :: (a -> b) -> (b -> c) -> a -> c"

  it "lists primitives that mention a type anywhere in their signature" $
    case help "Regex" of
      Left err  -> expectationFailure (T.unpack err)
      Right out -> T.lines out `shouldSatisfy` all ("Regex" `T.isInfixOf`)

  it "includes a type nested inside a function argument" $
    case help "Char" of
      Left err  -> expectationFailure (T.unpack err)
      Right out -> T.lines out `shouldContain` ["map :: (Char -> Char) -> String -> String"]

  it "prints pair types" $
    (take 1 . T.lines <$> help "zip") `shouldBe` Right ["zip :: [a] -> [b] -> [(a, b)]"]

  it "prints a documented primitive's signature, description and examples" $
    help "swap" `shouldBe` Right
      "swap :: (a, b) -> (b, a)\n\n\
      \Swaps the order of the two values in a pair\n\n\
      \>>> swap((\"1\", 2))\n(2, \"1\")"

  it "a type query lists only signatures, even for documented primitives" $
    case help "Pair" of
      Left err  -> expectationFailure (T.unpack err)
      Right out -> do
        T.lines out `shouldContain` ["swap :: (a, b) -> (b, a)"]
        T.lines out `shouldSatisfy` all (" :: " `T.isInfixOf`)

  it "Pair lists every primitive mentioning a pair, without the parser-only names" $
    case help "Pair" of
      Left err  -> expectationFailure (T.unpack err)
      Right out -> do
        let ls = T.lines out
        ls `shouldSatisfy` all ("(" `T.isInfixOf`)
        ls `shouldContain` ["zip :: [a] -> [b] -> [(a, b)]"]
        map (T.takeWhile (/= ' ')) ls `shouldNotContain` ["(,)", "\\(,)"]

  it "reports an unknown query" $
    help "doesNotExist" `shouldBe` Left "no primitive or type named doesNotExist"
