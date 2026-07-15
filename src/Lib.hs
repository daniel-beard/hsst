module Lib
  ( runProgram
  , runProgramWithLog
  ) where

import Data.Bifunctor (first)
import qualified Data.Text as T

import Parser   (parseProgram)
import Resolve  (resolve)
import Diagnostics (renderDiagnostic)
import Infer    (inferProgram)
import Interp   (Interp, InterpR(..), InterpW, runInterp)
import Elaborate (elaborateClosed)
import Core
import Eval     (Env(..), eval, render)
import qualified Prims


-- parse, resolve, infer, elaborate. Convert to our GADT.
prepare :: T.Text -> Either T.Text (Typed (Term ()))
prepare src = do
  uterm  <- first T.pack (parseProgram (T.unpack src))
  ixterm <- first (renderDiagnostic src) (resolve Prims.primNames uterm)
  ann    <- first (renderDiagnostic src) (inferProgram Prims.primSchemes ixterm)
  first (renderDiagnostic src) (elaborateClosed ann)

-- Run program against stdin string.
-- Returns an error message, or the program output.
-- Writer (InterpW) is discarded, use `runProgramWithLog` to get it.
runProgram :: T.Text -> T.Text -> Either T.Text T.Text
runProgram src stdin_ = fst <$> runProgramWithLog src stdin_

runProgramWithLog :: T.Text -> T.Text -> Either T.Text (T.Text, InterpW)
runProgramWithLog src stdin_ = do
  Typed ty term <- prepare src
  Right (runInterp (InterpR stdin_) (run ty term))
  where
    run :: Ty t -> Term () t -> Interp T.Text
    run ty term = case ty of
      -- If this program is String -> a, feed it stdin.
      TyStrT :-> ret -> do
        f <- eval ENil term
        r <- f stdin_
        pure (render ret r)
      -- Otherwise, eval as is
      _ -> do
        v <- eval ENil term
        pure (render ty v)

