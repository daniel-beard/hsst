module Lib
  ( runProgram
  , runProgramWithLog
  ) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map

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
prepare :: String -> Either String (Typed (Term ()))
prepare src = do
  uterm  <- parseProgram src
  ixterm <- first (renderDiagnostic src) (resolve Prims.primNames uterm)
  ann    <- first (renderDiagnostic src) (inferProgram (Map.fromList Prims.primSchemes) ixterm)
  first (renderDiagnostic src) (elaborateClosed ann)

-- Run program against stdin string. 
-- Returns an error message, or the program output.
-- Writer (InterpW) is discarded, use `runProgramWithLog` to get it.
runProgram :: String -> String -> Either String String
runProgram src stdin_ = fst <$> runProgramWithLog src stdin_

runProgramWithLog :: String -> String -> Either String (String, InterpW)
runProgramWithLog src stdin_ = do
  Typed ty term <- prepare src
  Right (runInterp (InterpR stdin_) (run ty term))
  where 
    run :: Ty t -> Term () t -> Interp String
    run ty term = case ty of
      -- If this program is String -> a, feed it stdin.
      TyListT TyCharT :-> ret -> do
        f <- eval ENil term
        r <- f stdin_
        pure (render ret r)
      -- Otherwise, eval as is
      _ -> do
        v <- eval ENil term
        pure (render ty v)

