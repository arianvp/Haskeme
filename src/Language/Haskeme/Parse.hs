module Language.Haskeme.Parse
( parse
, ParseError
) where

import           Control.Applicative    (pure, (*>), (<$>), (<*), (<*>), (<|>))
import           Language.Haskeme.AST
import qualified Language.Haskeme.Error as E (SchemeError (..), throwError)
import qualified Language.Haskeme.Lex   as L
import qualified Text.Parsec            as P hiding ((<|>))
import           Text.Parsec.Error      (ParseError)
import           Text.Parsec.String


-- literals
bool, integer, stringLiteral, atom :: Parser Expr
bool          = Bool    <$> L.bool
integer       = Integer <$> L.integer
stringLiteral = String  <$> L.stringLiteral
atom          = Atom    <$> (L.identifier <|> L.builtIn)

-- listlikes
list,dottedList :: Parser Expr
list        = L.parens $ List       <$> P.many expr
dottedList  = L.parens $ DottedList <$> P.manyTill expr (L.symbol ".")
                                    <*> expr

quote, quasiquote, unquote, unquoteSplicing :: Parser Expr
quote            = (L.symbol "'")  `prefixWith` "quote"
quasiquote       = (L.symbol "`")  `prefixWith` "quasiquote"
unquote          = (L.symbol ",")  `prefixWith` "unquote"
unquoteSplicing  = (L.symbol ",@") `prefixWith` "unquote-splicing"

a `prefixWith` b = List . ([Atom b] ++) . (:[]) <$> (a *> expr)

expr :: Parser Expr
expr =  quote
    <|> bool
    <|> quasiquote
    <|> P.try unquoteSplicing
    <|> unquote
    <|> P.try list
    <|> dottedList
    <|> stringLiteral
    <|> P.try integer
    <|> atom


parser = L.whiteSpace *> (expr) <* P.eof

parse :: String -> Either E.SchemeError Expr
parse  = (either (E.throwError. E.Parser) return) . P.parse parser "Haskeme"
