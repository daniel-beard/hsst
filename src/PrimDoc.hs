-- | Help text for primitives, shown by `hsst -h <primitive>`.
-- Written with the `doc` quasi quoter in "QQ".
module PrimDoc
  ( PrimDoc(..)
  , Example(..)
  , mkPrimDoc
  ) where

import qualified Data.Text as T

data PrimDoc = PrimDoc
  { docSummary  :: T.Text
  , docExamples :: [Example]
  } deriving (Eq, Show)

-- | A closed hsst program (no stdin) and its result, rendered Show-style.
-- Every example is run by the test suite.
data Example = Example
  { exSource :: T.Text
  , exResult :: T.Text
  } deriving (Eq, Show)

mkPrimDoc :: String -> [(String, String)] -> PrimDoc
mkPrimDoc summary exs =
  PrimDoc (T.pack summary) [ Example (T.pack s) (T.pack r) | (s, r) <- exs ]
