module Shell
  ( parse
  , parseFile
  , translate
  , translateExp
  ) where

import Language.Haskell.Meta.Parse qualified as Parse
import Language.Haskell.TH qualified as TH

import Lambda

translateExp :: TH.Exp -> Maybe Term
translateExp = \case
  TH.VarE name ->
    Just . Var $ translateName name
  TH.UnboundVarE name ->
    Just . Var $ translateName name
  TH.AppE expr1 expr2 ->
    Call <$> translateExp expr1 <*> translateExp expr2
  TH.ParensE expr ->
    translateExp expr
  TH.LamE pats body -> do
    body' <- translateExp body
    params <- mapM extractParam pats
    Just $ foldr Fun body' params
  TH.LetE decs body -> do
    body' <- translateExp body
    bindings <- mapM extractBinding decs
    Just $ foldr apply body' bindings
  _ ->
    Nothing
  where
    translateName = Symbol . TH.nameBase
    extractParam = \case
      TH.VarP name -> Just (translateName name)
      _ -> Nothing
    extractBinding = \case
      TH.ValD (TH.VarP name) (TH.NormalB expr) _whereDecs -> do
        term <- translateExp expr
        Just (translateName name, term)
      TH.FunD name [TH.Clause pats (TH.NormalB expr) _whereDecs] -> do
        term <- translateExp expr
        params <- mapM extractParam pats
        Just (translateName name, foldr Fun term params)
      _ -> Nothing
    apply (name, value) term =
      Call (Fun name term) value

translate :: (TH.Quote m) => m TH.Exp -> m (Maybe Term)
translate = fmap translateExp

parse :: String -> Maybe Term
parse = either (const Nothing) translateExp . Parse.parseExp

parseFile :: String -> IO (Maybe Term)
parseFile = fmap parse . readFile
