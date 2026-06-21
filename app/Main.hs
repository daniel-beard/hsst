module Main (main) where

import Control.Exception (SomeException, catch)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdin, hIsTerminalDevice)
import System.Process (readProcess)

import Lib (runProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> do
      -- When stdin is piped or passed in, use it.
      -- isTty means nothing to read from stdin, use pasteboard contents in that case.
      isTty  <- hIsTerminalDevice stdin
      stdin_ <- if isTty then readPasteboard else getContents
      case runProgram src stdin_ of
        Right out -> putStrLn out
        Left  err -> hPutStrLn stderr err >> exitFailure
    _ -> do
      hPutStrLn stderr "usage: hsst '<program>'"
      exitFailure

-- pasteboard or empty string.
readPasteboard :: IO String
readPasteboard =
  readProcess "pbpaste" [] "" `catch` \(_ :: SomeException) -> pure ""
