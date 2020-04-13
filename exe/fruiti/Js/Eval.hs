{-# LANGUAGE BlockArguments #-}

module Js.Eval where

import Data.Map (foldMapWithKey)
import qualified Data.Text as Text
import qualified Js.Types as Js
import qualified Language.Fruit.Syntax.AST as AST
import System.Process.Typed (readProcess_, shell)
import Types

evalTerm :: MonadIO m => Env -> (AST.Term -> Js.Code) -> AST.Term -> m Js.Result
evalTerm env format e = do
  let code = reverse $ toText (format e) : formatEnv env
  putTextLn . unlines . fmap ("JS » " <>) $ code
  (out, _) <-
    readProcess_ . shell $
      "node -p '" <> toString (unlines code) <> "'"
  return . Js.Result . Text.stripEnd . decodeUtf8 $ out
  where
    formatEnv :: Env -> [Text]
    formatEnv = foldMapWithKey \(Var var) expr ->
      ["const " <> var <> " = " <> toText (format expr)]
