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

  it "reports an unknown query" $
    help "doesNotExist" `shouldBe` Left "no primitive or type named doesNotExist"
