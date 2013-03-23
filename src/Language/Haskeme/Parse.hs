module Language.Haskeme.Parse
(
parser
) where

import qualified Language.Haskeme.Lex as L
import Language.Haskeme.AST
import Text.Parsec as P
import Text.Parsec.String 
import Control.Applicative ((<$>), (<*>), (*>), (<*))

-- literals
bool, integer, stringLiteral, atom :: Parser Expr
bool          = Bool    <$> L.bool
integer       = Integer <$> L.integer
stringLiteral = String  <$> L.stringLiteral
atom          = Atom    <$> L.identifier

-- listlikes
list, vector, dottedList :: Parser Expr
list       = List       <$> L.parens       (expr `sepBy` L.whiteSpace)
vector     = Vector     <$> L.vectorParens (expr `sepBy` L.whiteSpace)
dottedList = DottedList <$> L.parens       (expr `endBy` L.whiteSpace)
                        <*> (L.dot *> L.whiteSpace *> expr)
-- quotes
quote, quasiquote, unquote, unquoteSplicing :: Parser Expr
quote            = L.apostrophe  `prefixWith` "quote"
quasiquote       = L.backtick    `prefixWith` "quasiquote"
unquote          = L.comma       `prefixWith` "unquote"
unquoteSplicing  = P.string ",@" `prefixWith` "unquote-splicing"

a `prefixWith` b = List . ([Atom b] ++) . (:[]) <$> (a *> expr)

expr :: Parser Expr
expr =  quote          
    <|> quasiquote     
    <|> try unquoteSplicing
    <|> unquote        
    <|> try list
    <|> dottedList
    <|> vector
    <|> stringLiteral
    <|> integer
    <|> bool
    <|> atom

parser = L.whiteSpace *> (P.many expr) <* P.eof




