module TestUtil (shouldBeRight, shouldBeLeftContaining, outputs) where

import qualified Data.Text as T
import Lib (runProgram)
import Test.Hspec

-- | Expects a 'Right' equal to the given value.
-- On a 'Left', prints the error text as-is instead of a shown 'Left'.
shouldBeRight :: (HasCallStack, Eq a, Show a) => Either T.Text a -> a -> Expectation
shouldBeRight result expected = case result of
  Right r  -> r `shouldBe` expected
  Left err -> expectationFailure ("expected " ++ show expected ++ ", got an error:\n" ++ T.unpack err)

-- | Expects a 'Left' whose message contains the given substring.
shouldBeLeftContaining :: HasCallStack => Either T.Text T.Text -> T.Text -> Expectation
shouldBeLeftContaining result needle = case result of
  Left err -> T.unpack err `shouldContain` T.unpack needle
  Right r  -> expectationFailure ("expected an error containing " ++ show needle ++ ", got " ++ show r)

-- | Runs a program with empty stdin and expects this output.
-- For programs that read stdin, use 'runProgram' with 'shouldBeRight'.
outputs :: HasCallStack => T.Text -> T.Text -> Expectation
outputs src expected = runProgram src "" `shouldBeRight` expected

infix 1 `shouldBeRight`, `shouldBeLeftContaining`, `outputs`
