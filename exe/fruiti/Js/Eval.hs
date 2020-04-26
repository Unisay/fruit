{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Js.Eval where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.Fruit.Js.Syntax
import System.Process.Typed (readProcess_, shell)

type Env = Map Var Code

newtype Code = Code Text
  deriving newtype (Show, ToText, IsString)

newtype Result = Result Text
  deriving newtype (Show, ToText)

builtInEnv :: Env
builtInEnv =
  Map.mapKeys Var . fmap Code . Map.fromList $
    [ ("plus", "x=>y=>(x+y)"),
      ("minus", "x=>y=>(x-y)"),
      ("mul", "x=>y=>(x*y)"),
      ("div", "x=>y=>(x/y)"),
      ("pow", "x=>y=>(x**y)")
    ]

evalTerm :: MonadIO m => Env -> Code -> m Result
evalTerm env jsCode = do
  let code = reverse $ toText jsCode : formatEnv (Map.union builtInEnv env)
      unJsLines = Text.intercalate ";\n"
      js = unJsLines code
  putTextLn "═════ JS code ═════"
  putTextLn js
  evalRawText js
  where
    formatEnv :: Env -> [Text]
    formatEnv = Map.foldMapWithKey \var code ->
      ["const " <> toText var <> " = " <> toText code]

evalRawText :: MonadIO m => Text -> m Result
evalRawText code = do
  (out, _) <- readProcess_ . shell $ "node -p '" <> toString code <> "'"
  return . Result . Text.stripEnd . decodeUtf8 $ out
