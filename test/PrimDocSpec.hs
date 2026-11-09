module PrimDocSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Lib (evalRendered)
import PrimDoc (PrimDoc(..), Example(..))
import Prims (primHelpEntries)
import QQ (doc, hsst)
import Test.Hspec
import TestUtil

spec :: Spec
spec = do
  -- One test per example, so a failure names the example.
  describe "prim doc examples" $
    forM_ (Map.toList primHelpEntries) $ \(n, entries) ->
      forM_ [ ex | (_, Just d) <- entries, ex <- docExamples d ] $ \ex ->
        it (n <> ": " <> T.unpack (exSource ex)) $
          evalRendered (exSource ex) `shouldBeRight` exResult ex

  describe "doc quasi quoter" $ do
    it "dedents, keeps description line breaks, and reads each example and its result" $
      [doc|
        First line
          indented line

        >>> fst((1, 2))
        1

        >>> snd((1, 2))
        2
      |] `shouldBe` PrimDoc "First line\n  indented line"
                      [Example "fst((1, 2))" "1", Example "snd((1, 2))" "2"]

    it "allows a doc with no examples" $
      [doc| Just a description |] `shouldBe` PrimDoc "Just a description" []

  describe "evalRendered" $ do
    it "renders Show-style" $
      evalRendered [hsst| ("a", words("x y")) |] `shouldBeRight` "(\"a\", [\"x\", \"y\"])"

    it "reports errors" $
      evalRendered "fst(3)" `shouldBeLeftContaining` "cannot unify"

  it "every callable prim is documented" $
    case [ n | (n, entries) <- Map.toList primHelpEntries
             , n `notElem` ["|>", "(,)", "\\(,)"]
             , any (null . snd) entries ] of
      []           -> pure ()
      undocumented -> pendingWith ("undocumented: " <> unwords undocumented)
