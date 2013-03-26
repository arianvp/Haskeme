module Language.Haskeme.AST
( Expr (..)
, unwords'
) where

data Expr = Bool Bool
          | String String
          | Integer Integer
          | Atom String
          | List [Expr]
          | Vector [Expr]
          | DottedList [Expr] Expr
          deriving Show
{-- instance Show Expr where
    show (Bool True)      = "#t"
    show (Bool False)     = "#f"
    show (String s)       = "\"" ++ s ++ "\""
    show (Integer i)      = show i
    show (Atom a)         = a
    show (List l)         = "(" ++ unwords' l ++ ")"
    show (Vector v)       = "#(" ++ unwords' v ++ ")"
    show (DottedList h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
--}
unwords' :: [Expr] -> String
unwords' = unwords . map show



