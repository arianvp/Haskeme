module Language.Haskeme.Lex
( identifier
, reserved 
, operator 
, reservedOp 
, charLiteral 
, stringLiteral 
, natural 
, integer 
, float 
, naturalOrFloat 
, decimal 
, hexadecimal 
, octal 
, bool 
, symbol 
, lexeme 
, whiteSpace 
, parens 
, vectorParens 
, semi 
, comma 
, colon 
, apostrophe 
, backtick 
, hashtag 
, splicer 
, dot 
)
where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Data.Char
import Control.Applicative hiding ((<|>), many)

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef
        { T.commentLine     = ";"
        , T.identStart      = initial
        , T.identLetter     = subsequent
        , T.reservedOpNames = ["+", "-"]
        , T.reservedNames   = [ "quote", "lambda", "if"
                              , "set!", "begin", "cond", "and", "or", "case"
                              , "let", "let*", "letrec", "do", "delay"
                              , "quasiquote", "else", "=>", "define"
                              , "unquote", "unquote-splicing"
                              ]
        }
        where initial     = letter <|> initial'
              initial'    = oneOf "!$%&*/:<=>?^_~"
              subsequent  = initial <|> digit <|> subsequent'
              subsequent' = oneOf"+-.@"


identifier = T.identifier lexer
reserved = T.reserved lexer
operator = T.operator lexer
reservedOp = T.reservedOp lexer
charLiteral = T.charLiteral lexer
stringLiteral = T.stringLiteral lexer
natural = T.natural lexer
integer = T.integer lexer
float = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
decimal = T.decimal lexer
hexadecimal = T.hexadecimal lexer
octal = T.octal lexer
bool :: Parser Bool
bool = (== 't') <$> (char '#' *> oneOf "tf")
symbol = T.symbol lexer
lexeme = T.lexeme lexer
whiteSpace = T.whiteSpace lexer
parens = T.parens lexer 
vectorParens = between (symbol "#(") (char ')')
semi = T.semi lexer
comma = T.comma lexer
colon = T.colon lexer
apostrophe = lexeme $ char '\''
backtick = lexeme $ char ','
hashtag = lexeme $ char '#'
splicer = symbol ",@"
dot = T.dot lexer

