module Language.Haskeme.Lex
( identifier
, charLiteral
, stringLiteral
, bool
, symbol
, whiteSpace
, parens
, builtIn
, integer
)
where

import           Control.Applicative  hiding (many, (<|>))
import           Data.Char            (digitToInt)
import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as T

lexer = T.makeTokenParser emptyDef

identifier :: Parser String
identifier = lexeme $ (:) <$> initial <*> many subsequent
           where initial     = letter <|> initial'
                 initial'    = oneOf "!$%&*/:<=>?^_~"
                 subsequent  = initial <|> digit <|> subsequent'
                 subsequent' = oneOf "+-.@"

charLiteral :: Parser Char
charLiteral = T.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

natural :: Parser Integer
natural =  char '0' *> pure 0
       <|> number

integer :: Parser Integer
integer =  lexeme $ (char '-') *> ((0-) <$> natural)
       <|> (char '+') *> natural
       <|> natural




bool :: Parser Bool
bool = lexeme $ (== 't') <$> (char '#' *> oneOf "tf" <?> "#t or #f")

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

symbol :: String -> Parser String
symbol = T.symbol lexer

whiteSpace :: Parser ()
whiteSpace  = skipMany (simpleSpace <|> comment)
            where simpleSpace = skipMany1 space
                  comment :: Parser ()
                  comment     = char ';' *> (skipMany $ satisfy (/= '\n'))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser Integer
number =  lexeme $ do digits <- many1 digit
                      let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
                      seq n $ return n


builtIn :: Parser String
builtIn = lexeme $ (:[]) <$> oneOf "+-/*"
