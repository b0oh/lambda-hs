module Lambda
  ( Term (..)
  , Symbol (..)

    -- * reduction
  , normalize
  , weakNormalize
  , reduceOnce
  , reduceFull
  , substitute
  , substitute'
  , rename

    -- * names
  , isFree
  , names
  , freeNames
  , freshName

    -- * helpers
  , zero
  , one
  , two
  ) where

import Data.Char qualified as Char
import Data.List qualified as List

newtype Symbol = Symbol String
  deriving (Eq, Ord, Show)

data Term
  = Var Symbol
  | Fun Symbol Term
  | Call Term Term
  deriving (Eq, Show)

normalize :: Term -> Term
normalize = \case
  Call callee argument ->
    case weakNormalize callee of
      Fun name body ->
        normalize $ substitute name argument body
      whnfCallee ->
        Call (normalize whnfCallee) (normalize argument)
  Fun name body ->
    Fun name $ normalize body
  term ->
    term

weakNormalize :: Term -> Term
weakNormalize = \case
  Call callee argument ->
    case weakNormalize callee of
      Fun name body ->
        weakNormalize $ substitute name argument body
      whnfCallee ->
        Call whnfCallee argument
  term ->
    term

substitute :: Symbol -> Term -> Term -> Term
substitute name newTerm term =
  case term of
    Call callee argument ->
      Call (substitute name newTerm callee) (substitute name newTerm argument)
    Fun name' body
      | name' == name ->
          term
      | isFree name' newTerm && isFree name body ->
          Fun fresh $ substitute name newTerm $ rename name' fresh body
      | otherwise ->
          Fun name' $ substitute name newTerm body
      where
        fresh = freshName name' $ freeNames newTerm `List.union` names body
    Var name'
      | name' == name ->
          newTerm
      | otherwise ->
          term

substitute' :: Symbol -> Term -> Term -> Term
substitute' name newTerm term =
  case term of
    Call callee arg ->
      Call (substitute name newTerm callee) (substitute name newTerm arg)
    Fun name' _body
      | name' == name ->
          term
    Fun name' body ->
      Fun name' (substitute name newTerm body)
    Var name'
      | name' == name ->
          newTerm
    _ ->
      term

rename :: Symbol -> Symbol -> Term -> Term
rename name newName term =
  case term of
    Call callee argument ->
      Call (rename name newName callee) (rename name newName argument)
    Fun name' _
      | name == name' ->
          term
    Fun name' body ->
      Fun name' $ rename name newName body
    Var name'
      | name == name' ->
          Var newName
    _ ->
      term

isFree :: Symbol -> Term -> Bool
isFree name = \case
  Call callee argument ->
    isFree name callee || isFree name argument
  Fun name' body ->
    name /= name' && isFree name body
  Var name' ->
    name == name'

names :: Term -> [Symbol]
names = \case
  Call callee argument ->
    names callee `List.union` names argument
  Fun name body ->
    [name] `List.union` names body
  Var name ->
    [name]

freshName :: Symbol -> [Symbol] -> Symbol
freshName (Symbol name) taken =
  go (1 :: Int)
  where
    stem = List.dropWhileEnd Char.isDigit name
    go number
      | candidate `elem` taken =
          go $ number + 1
      | otherwise =
          candidate
      where
        candidate = Symbol $ stem <> show number

freeNames :: Term -> [Symbol]
freeNames =
  go []
  where
    go bound = \case
      Call callee argument ->
        go bound callee `List.union` go bound argument
      Fun name body ->
        go (name : bound) body
      Var name ->
        [name | name `notElem` bound]

reduceOnce :: Term -> Maybe Term
reduceOnce = \case
  Call (Fun name body) arg ->
    Just $ substitute name arg body
  Call callee arg ->
    case reduceOnce callee of
      Just callee' ->
        Just $ Call callee' arg
      Nothing ->
        Call callee <$> reduceOnce arg
  Fun name body ->
    Fun name <$> reduceOnce body
  _ ->
    Nothing

reduceFull :: Term -> Term
reduceFull term =
  maybe term reduceFull $ reduceOnce term

zero :: Term
zero =
  Fun (Symbol "next") $
    Fun (Symbol "init") $
      Var (Symbol "init")

one :: Term
one =
  Fun (Symbol "next") $
    Fun (Symbol "init") $
      Call (Var (Symbol "next")) (Var (Symbol "init"))

increment :: Term
increment =
  Fun (Symbol "num") $
    Fun (Symbol "next") $
      Fun (Symbol "init") $
        Call (Var (Symbol "next")) $
          Call (Call (Var (Symbol "num")) (Var (Symbol "next"))) $
            Var (Symbol "init")

two :: Term
two = Call increment one
