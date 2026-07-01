-- The interpreter monad.
-- Run inside a pure RWS monad:
--
-- Reader (InterpR): 
--  - immutable run context. 
--  - Only contains original stdin right now.
-- Writer (InterpW):
--  - append only writer monad. 
--  - `tee` currently writes to this, which is then printed from `main`. 
--     (Will want to make this proper IO eventually).
-- State (InterpS):
--  - Interpreter state, read / write.
--  - regex group captures are stored here
--
-- For now, there's no IO here. When adding in future, will have to use `RWST .. IO`
module Interp
  ( Interp
  , InterpR(..)
  , InterpW
  , InterpS(..)
  , runInterp
  ) where

import Control.Monad.RWS.Strict (RWS, runRWS)

-- Read only interpreter context
newtype InterpR = InterpR { stdinText :: String }

-- Append only output logs e.g. `tee`. 
type InterpW = [String]

-- Mutable interpreter state
newtype InterpS = InterpS 
  { 
    -- Last matched regex groups
    -- Index 0 is the full match
    -- 1 onwards are the group matches
    lastRegexGroupMatches :: [String]
  }

type Interp = RWS InterpR InterpW InterpS

-- Run against a context, returns the result and the writer.
-- Final state is discarded for now.
runInterp :: InterpR -> Interp a -> (a, InterpW)
runInterp r m = let (a, _s, w) = runRWS m r (InterpS []) in (a, w)

