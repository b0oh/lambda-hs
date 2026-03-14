module Codec
  ( decode
  , decodeNat
  , encodeNat
  ) where

import Lambda

data Decoded
  = Nat Int
  deriving (Eq, Show)

decode :: Term -> Maybe Decoded
decode = decodeNat

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
