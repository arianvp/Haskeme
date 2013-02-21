module Main(main) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Data.Char
import Control.Applicative hiding ((<|>), many)

-- parseDottedList :: Parser Value
-- parseDottedList = DottedList <$> (endBy parseExpr spaces) <*> (char '.' *> spaces *> parseExpr)    

-- parseQuoted :: Parser Value
-- parseQuoted = List <$> ((Atom "quote") :) <$> (: []) <$> (char '\'' *> parseExpr)



-- http://rose-r5rs.googlecode.com/hg/doc/r5rs-grammar.html
lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
        { T.commentLine     = ";"
        , T.identStart      = initial
        , T.identLetter     = subsequent
        , T.reservedOpNames = ["+", "-"]
        , T.reservedNames   = ["quote", "lambda", "if"
                            ,"set!", "begin", "cond", "and", "or", "case"
                            , "let", "let*", "letrec", "do", "delay"
                            , "quasiquote", "else", "=>", "define"
                            , "unquote", "unquote-splicing"
                            ]
        }
        where initial     = letter <|> initial'
              initial'    = oneOf "!$%&*/:<=>?^_~"
              subsequent  = initial <|> digit <|> subsequent'
              subsequent' = oneOf"+-.@"

data Expr = Bool Bool
          | String String
          | Integer Integer
          | Atom String
          | List [Expr]
          | DottedList [Expr] Expr deriving (Show)

whiteSpace = T.whiteSpace lexer
parens = T.parens lexer

bool :: Parser Expr
bool =  Bool <$> (char '#' *> (char 't' *> pure True  <|> char 'f' *> pure False))

stringLiteral :: Parser Expr
stringLiteral = String <$> T.stringLiteral lexer

integer :: Parser Expr
integer = Integer <$> T.integer lexer

list :: Parser Expr
list = List <$> expr `sepBy` whiteSpace

atom :: Parser Expr
atom = Atom <$> T.identifier lexer

expr :: Parser Expr
expr =  (parens list) <|> stringLiteral <|> integer <|> bool <|> atom

main = getLine >>= parseTest (whiteSpace *> expr <* eof)



