module Lib
  ( runProgram
  , runProgramWithLog
  ) where

import Data.Bifunctor (first)
import qualified Data.Text as T

import Parser   (parseProgram)
import Resolve  (resolve)
import Diagnostics (Span, Diagnostic(..), renderDiagnostic)
import Syntax   (annSpan)
import Infer    (inferProgram)
import Interp   (Interp, InterpR(..), InterpW, runInterp)
import Elaborate (elaborateClosed)
import Core
import Eval     (Env(..), eval, renderOutput)
import qualified Prims


-- parse, resolve, infer, elaborate. Convert to our GADT.
prepare :: T.Text -> Either T.Text (Typed (Term ()))
prepare src = do
  uterm  <- first T.pack (parseProgram (T.unpack src))
  ixterm <- first (renderDiagnostic src) (resolve Prims.primNames uterm)
  ann    <- first (renderDiagnostic src) (inferProgram Prims.primSchemes ixterm)
  typed  <- first (renderDiagnostic src) (elaborateClosed ann)
  first (renderDiagnostic src) (checkRunnable (annSpan ann) typed)
  pure typed

-- A program must be a value or a stdin filter, and its output must be printable.
-- Rejects e.g. `take` (missing an argument) or `/foo/` (a bare regex).
checkRunnable :: Span -> Typed (Term ()) -> Either Diagnostic ()
checkRunnable sp (Typed ty _) = case ty of
  TyStrT :-> ret          -> printable ret
  TyListT TyCharT :-> ret -> printable ret
  _ :-> _ ->
    failWith ("program has type " <> showTy ty
                <> ", which can't run: it must be a value or a stdin filter (String -> a)")
             "not runnable"
  _ -> printable ty
  where
    printable :: Ty r -> Either Diagnostic ()
    printable out = case unprintable out of
      Nothing   -> Right ()
      Just hint ->
        failWith ("program output has type " <> showTy out <> ", which can't be printed; " <> hint)
                 "can't print"

    failWith msg lbl = Left Diagnostic { diagMessage = msg, diagSpan = sp, diagLabel = lbl }

-- Hint for the first part of an output type that has no printed form, if any.
unprintable :: Ty t -> Maybe T.Text
unprintable ty = case ty of
  TyArrT _ _ -> Just "is it missing an argument?"
  TyRegexT   -> Just "use it with matches or match"
  TyListT a  -> unprintable a
  _          -> Nothing

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
        pure (renderOutput ret r)
      -- [Char] -> a, from a list-generic prim with no String overload. Feed stdin as chars.
      TyListT TyCharT :-> ret -> do
        f <- eval ENil term
        r <- f (T.unpack stdin_)
        pure (renderOutput ret r)
      -- Otherwise, eval as is
      _ -> do
        v <- eval ENil term
        pure (renderOutput ty v)

