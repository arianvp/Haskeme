module Language.Haskeme.AST(
Expr (..)
) where

data Expr = Bool Bool
          | String String
          | Integer Integer
          | Atom String
          | List [Expr]
          | Vector [Expr]
          | DottedList [Expr] Expr
          | Error String

instance Show Expr where
    show (Bool True)      = "#t"
    show (Bool False)     = "#f"
    show (String s)       = "\"" ++ s ++ "\""
    show (Integer i)      = show i
    show (Atom a)         = a
    show (List l)         = "(" ++ unwords' l ++ ")"
    show (Vector v)       = "#(" ++ unwords' v ++ ")"
    show (DottedList h t) = "(" ++ unwords' h ++ " . " ++ show t ++ ")"
    show (Error s)        = s
unwords' :: [Expr] -> String
unwords' = unwords . map show



