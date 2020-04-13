module ReplM where

import System.Console.Repline (HaskelineT, ReplOpts, evalReplOpts)
import Types

type Repl = HaskelineT (StateT Env IO)

runReplM :: ReplOpts (StateT Env IO) -> IO ()
runReplM opts = evalStateT (evalReplOpts opts) mempty
