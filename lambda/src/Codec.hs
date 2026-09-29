module Codec
  ( decode
  , decodeBool
  , decodeNat
  , decodePair
  , encodeBool
  , encodeNat
  , encodePair
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

import Lambda

data Decoded
  = Bool Bool
  | Nat Int
  | Pair Decoded Decoded
  | Term Term
  deriving (Eq, Show)

decode :: Term -> Maybe Decoded
decode term =
  decodeBool term <|> decodePair term <|> decodeNat term

decodeBool :: Term -> Maybe Decoded
decodeBool = \case
  Fun _ (Fun f (Var x))
    | x == f ->
        Just $ Bool False
  Fun t (Fun f (Var x))
    | t /= f && x == t ->
        Just $ Bool True
  _ ->
    Nothing

decodePair :: Term -> Maybe Decoded
decodePair term =
  case term of
    Fun pair (Call (Call (Var pair') first) second)
      | pair == pair' ->
          Just $ Pair first' second'
      where
        first' = fromMaybe (Term first) $ decode first
        second' = fromMaybe (Term second) $ decode second
    _ ->
      Nothing

decodeNat :: Term -> Maybe Decoded
decodeNat = \case
  Fun n (Fun i (Var i'))
    | n /= i && i == i' ->
        Just $ Nat 0
  Fun n (Fun i (Call (Var n') body)) | n == n' -> do
    decoded <- decodeNat $ Fun n $ Fun i body
    case decoded of
      Nat num ->
        Just $ Nat $ num + 1
      _ ->
        Nothing
  _ ->
    Nothing

encodeBool :: Bool -> Term
encodeBool = \case
  True ->
    Fun t $ Fun f $ Var t
  False ->
    Fun t $ Fun f $ Var f
  where
    t = Symbol "t"
    f = Symbol "f"

encodeNat :: Int -> Term
encodeNat = \case
  0 ->
    Fun n $ Fun i $ Var i
  num ->
    case encodeNat (num - 1) of
      Fun n' (Fun i' body)
        | n == n' && i == i' ->
            Fun n $ Fun i $ Call (Var n) body
      _ ->
        error "unreachable"
  where
    n = Symbol "n"
    i = Symbol "i"

encodePair :: Term -> Term -> Term
encodePair first second =
  Fun pair $ Call (Call (Var pair) first) second
  where
    taken = freeNames first <> freeNames second
    pair
      | Symbol "pair" `elem` taken = freshName (Symbol "pair") taken
      | otherwise = Symbol "pair"
