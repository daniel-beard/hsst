module Main (main) where

import Control.Exception (SomeException, catch)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, stdin, hIsTerminalDevice)
import System.Process (readProcess)

import Lib (runProgramWithLog)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> do
      -- When stdin is piped or passed in, use it.
      -- isTty means nothing to read from stdin, use pasteboard contents in that case.
      isTty  <- hIsTerminalDevice stdin
      stdin_ <- if isTty then readPasteboard else TIO.getContents
      -- For now, eval is still pure.
      -- tee's log goes to stderr, program result to stdout
      case runProgramWithLog (T.pack src) stdin_ of
        Right (out, logs) -> do
          mapM_ (TIO.hPutStrLn stderr) logs
          TIO.putStrLn out
        Left  err -> TIO.hPutStrLn stderr err >> exitFailure
    _ -> do
      TIO.hPutStrLn stderr "usage: hsst '<program>'"
      exitFailure

-- pasteboard or empty string.
readPasteboard :: IO T.Text
readPasteboard =
  (T.pack <$> readProcess "pbpaste" [] "") `catch` \(_ :: SomeException) -> pure ""
