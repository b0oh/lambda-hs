module Printer (toString) where

import Lambda

toString :: Term -> String
toString = \case
  Call callee argument ->
    case argument of
      Call _ _ ->
        parenthesizeFun callee <> " (" <> toString argument <> ")"
      _ ->
        parenthesizeFun callee <> " " <> parenthesizeFun argument
    where
      parenthesizeFun term =
        case term of
          Fun _ _ -> "(" <> toString term <> ")"
          _ -> toString term
  Fun (Symbol name) body ->
    "\\" <> name <> " -> " <> toString body
  Var (Symbol name) ->
    name
