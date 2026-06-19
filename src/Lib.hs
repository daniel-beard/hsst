module Lib
  ( runProgram
  ) where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map

import Parser   (parseProgram)
import Resolve  (resolve)
import Diagnostics (renderDiagnostic)
import Infer    (inferProgram)
import Elaborate (elaborateClosed)
import Core
import Eval     (Env(..), eval, render)
import qualified Prims

-- Run a program string against a stdin string. Returns either an error
-- message or the rendered output.
runProgram :: String -> String -> Either String String
runProgram src stdin_ = do
  uterm  <- parseProgram src
  ixterm <- first (renderDiagnostic src) (resolve Prims.primNames uterm)
  ann    <- first (renderDiagnostic src) (inferProgram (Map.fromList Prims.primSchemes) ixterm)
  Typed ty term <- first (renderDiagnostic src) (elaborateClosed ann)
  case ty of
    -- If the program is a String -> a, feed it stdin.
    TyArrT TyStr ret ->
      let f = eval ENil term
      in pure (render ret (f stdin_))
    -- Otherwise print as-is.
    _ -> pure (render ty (eval ENil term))
