{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.Fruit.Js.Translator where

import Data.Generics.Uniplate (para)
import qualified Language.Fruit.Core as Core
import qualified Language.Fruit.Js.Syntax as JS
import qualified Unbound.LocallyNameless as U

translateFromCore :: Core.Term -> JS.Term
translateFromCore = U.runFreshM . para \case
  Core.LitInteger i
    | i > 9007199254740991 ->
      const . pure . JS.TermNumBigInt $ i
  Core.LitInteger i ->
    const . pure . JS.TermNumInt . fromIntegral $ i
  Core.LitFloating d ->
    const . pure . JS.TermNumFloating $ d
  Core.Var nam ->
    let var =
          if nam == "$$"
            then JS.Var "undefined"
            else name2var nam
     in -- TODO: replace invalid identifiers with undefined? Emit a warning?
        const . pure . JS.TermVar $ var
  Core.App {} -> \case
    [a, b] -> JS.TermCall <$> a <*> (pure <$> b)
    _ -> errSubTerms 2 "Core.App"
  Core.Lam bnd ->
    \case
      [t] -> do
        (p, _) <- U.unbind bnd
        JS.TermLambda (JS.PatternVar (name2var p)) <$> t
      _ -> errSubTerms 1 "Core.Lam"
  Core.Let _ bnd ->
    \case
      -- TODO: use let if inside statement block?
      [arg, body] -> do
        (p, _) <- U.unbind bnd
        lam <- JS.TermLambda (JS.PatternVar (name2var p)) <$> body
        JS.TermCall lam . pure <$> arg
      _ -> errSubTerms 2 "Core.Let"
  where
    name2var :: U.Name a -> JS.Var
    name2var = JS.Var . toText . U.name2String
    errSubTerms :: Int -> Text -> a
    errSubTerms n c =
      error $
        "translateFromCore: unexpected number of sub-terms received (/= "
          <> show n
          <> ") while para-morphing "
          <> c
          <> " constructor"
