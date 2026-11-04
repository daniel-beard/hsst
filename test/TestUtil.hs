module TestUtil (shouldBeLeftContaining) where

import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Test.Hspec

-- | Expects a 'Left' whose message contains the given substring.
shouldBeLeftContaining :: HasCallStack => Either T.Text T.Text -> T.Text -> Expectation
shouldBeLeftContaining result needle = case result of
  Left err -> T.unpack err `shouldContain` T.unpack needle
  Right r  -> expectationFailure ("expected an error containing " ++ show needle ++ ", got " ++ show r)

infix 1 `shouldBeLeftContaining`
