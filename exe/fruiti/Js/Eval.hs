module Js.Eval where

import qualified Data.Text as Text
import System.Process.Typed (readProcess_, shell)

evalExpr :: MonadIO m => Text -> m Text
evalExpr e = do
  (out, _) <- readProcess_ . shell $ "node -p '" <> toString e <> "'"
  return . Text.stripEnd . decodeUtf8 $ out
