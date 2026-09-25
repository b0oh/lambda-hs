module Lambda
  ( Term (..)
  , Symbol (..)
  , reduceOnce
  , reduceFull
  , substitute

    -- * helpers
  , zero
  , one
  , two
  ) where

newtype Symbol = Symbol String
  deriving (Eq, Ord, Show)

data Term
  = Var Symbol
  | Fun Symbol Term
  | Call Term Term
  deriving (Eq, Show)

substitute :: Symbol -> Term -> Term -> Term
substitute name newTerm term =
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
  case reduceOnce term of
    Just term' ->
      reduceFull term'
    _ ->
      term

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
