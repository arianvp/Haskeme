module Language.Haskeme.AST
( Expr (..)
) where

data Expr = Bool Bool
          | String String
          | Integer Integer
          | Atom String
          | List [Expr]
          | Vector [Expr]
          | DottedList [Expr] Expr

instance Show Expr where
    show (Bool True)      = "#t"
    show (Bool False)     = "#f"
    show (String s)       = "\"" ++ s ++ "\""
    show (Integer i)      = show i
    show (Atom a)         = a
    show (List l)         = "(" ++ (unwords . map show) l ++ ")"
    show (Vector v)       = "#(" ++ (unwords . map show) v ++ ")"
    show (DottedList h t) = "(" ++ (unwords . map show) h ++ " . " ++ show t ++ ")"




