module Diagnostics
  ( Span(..)
  , noSpan
  , mergeSpan
  , Diagnostic(..)
  , renderDiagnostic
  ) where

import qualified Data.Text as T

-- A source span as a half-open range of character offsets into the original
-- program text: [spanStart, spanEnd). These are the same offsets megaparsec
-- reports via `getOffset`, so the parser can record them directly.
data Span = Span
  { spanStart :: !Int
  , spanEnd   :: !Int
  } deriving (Eq, Show)

-- A placeholder span for synthetic AST nodes that don't correspond to any
-- source text (e.g. the `compose` inserted when desugaring `|>`). Such nodes
-- are never the subject of a diagnostic, so this is never rendered.
noSpan :: Span
noSpan = Span 0 0

-- The smallest span covering both operands, used to derive a composite node's
-- span from its children. A `noSpan` operand (e.g. the synthetic `compose`
-- inserted for `|>`) contributes nothing, so the real child's span wins.
mergeSpan :: Span -> Span -> Span
mergeSpan a b
  | a == noSpan = b
  | b == noSpan = a
  | otherwise   = Span (min (spanStart a) (spanStart b))
                       (max (spanEnd a)   (spanEnd b))

-- A structured error: a message, the span it points at, and a short label
-- drawn under the caret. The source text is supplied separately at render
-- time (it lives at the top level, in Lib), keeping passes source-agnostic.
data Diagnostic = Diagnostic
  { diagMessage :: T.Text
  , diagSpan    :: Span
  , diagLabel   :: T.Text
  } deriving (Eq, Show)

-- Render a diagnostic against the original source in a rustc-like frame:
--
--   error: unbound variable: foo
--    --> <arg>:1:10
--     |
--   1 | words |> foo
--     |          ^^^ not found in scope
--
renderDiagnostic :: T.Text -> Diagnostic -> T.Text
renderDiagnostic src (Diagnostic msg sp label) =
  let (lineNo, col, lineText) = locate src (spanStart sp)
      caretLen = max 1 (spanEnd sp - spanStart sp)
      gutter   = T.replicate (length (show lineNo)) " "
      pad      = T.replicate (col - 1) " "
      carets   = T.replicate caretLen "^"
      labelPart = if T.null label then "" else " " <> label
  in T.unlines
       [ "error: " <> msg
       , gutter <> "--> <arg>:" <> T.show lineNo <> ":" <> T.show col
       , gutter <> " |"
       , T.show lineNo <> " | " <> lineText
       , gutter <> " | " <> pad <> carets <> labelPart
       ]

-- Convert a character offset into a 1-based (line, column) plus the full text
-- of that line. O(n) in the source length, which is plenty for CLI snippets.
locate :: T.Text -> Int -> (Int, Int, T.Text)
locate src offset =
  let (before, _) = T.splitAt offset src
      nls        = T.count "\n" before
      lineStart  = case T.breakOnEnd "\n" before of
                     (pre, _) | T.null pre -> 0
                              | otherwise  -> T.length pre
      lineNo     = nls + 1
      col        = offset - lineStart + 1
      srcLines   = T.lines src
      lineText   = if lineNo >= 1 && lineNo <= length srcLines
                     then srcLines !! (lineNo - 1)
                     else ""
  in (lineNo, col, lineText)
