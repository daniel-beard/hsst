module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Lib (runProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> do
      stdin_ <- getContents
      case runProgram src stdin_ of
        Right out -> putStrLn out
        Left  err -> hPutStrLn stderr err >> exitFailure
    _ -> do
      hPutStrLn stderr "usage: hsst '<program>'"
      exitFailure
