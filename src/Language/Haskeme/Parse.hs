module Language.Haskeme.Parse
( parse
, ParseError
) where

import           Control.Applicative  (pure, (*>), (<$>), (<*), (<*>), (<|>))
import           Language.Haskeme.AST
import qualified Language.Haskeme.Lex as L
import qualified Language.Haskeme.Error as E (SchemeError (..), throwError)
import qualified Text.Parsec          as P hiding ((<|>))
import           Text.Parsec.Error    (ParseError)
import           Text.Parsec.String


-- literals
bool, integer, stringLiteral, atom :: Parser Expr
bool          = Bool    <$> L.bool
integer       = Integer <$> L.integer
stringLiteral = String  <$> L.stringLiteral
atom          = Atom    <$> (L.identifier <|> ((:[]) <$> L.builtIn))

-- listlikes
list, vector, dottedList :: Parser Expr
list' :: Parser [Expr]
list'       = L.parens $ expr `P.sepBy` L.whiteSpace
list        = List    <$> list'
vector      = Vector  <$> (L.reservedOp "#" *> list')
dottedList  = L.parens $ DottedList <$> expr `P.endBy` L.whiteSpace
                                    <*> (L.reservedOp "." *> expr)
-- quotes
quote, quasiquote, unquote, unquoteSplicing :: Parser Expr
quote            = (L.reservedOp "'")  `prefixWith` "quote"
quasiquote       = (L.reservedOp "`")  `prefixWith` "quasiquote"
unquote          = (L.reservedOp ",")  `prefixWith` "unquote"
unquoteSplicing  = (L.reservedOp ",@") `prefixWith` "unquote-splicing"

a `prefixWith` b = List . ([Atom b] ++) . (:[]) <$> (a *> expr)

expr :: Parser Expr
expr =  quote
    <|> P.try bool    -- # is also used for vectors.
    <|> quasiquote
    <|> unquote
    <|> unquoteSplicing
    <|> list
    <|> dottedList
    <|> vector
    <|> stringLiteral
    <|> integer
    <|> atom


parser = L.whiteSpace *> (expr) <* P.eof

parse :: String -> Either E.SchemeError Expr
parse  = (either (E.throwError. E.Parser) return) . P.parse parser "Haskeme"
