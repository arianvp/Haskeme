module Language.Haskeme.AST(
Expr(..)
) where

data Expr = Bool Bool
          | String String
          | Integer Integer
          | Atom String
          | List [Expr]
          | Vector [Expr]
          | DottedList [Expr] Expr
          deriving (Show)
