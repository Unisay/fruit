module ReplM where

import qualified JS
import System.Console.Repline (HaskelineT, ReplOpts, evalReplOpts)

type Repl = HaskelineT (StateT JS.Env IO)

runReplM :: ReplOpts (StateT JS.Env IO) -> IO ()
runReplM opts = evalStateT (evalReplOpts opts) mempty
