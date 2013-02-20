module Main(main) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Data.Char
import Control.Applicative hiding ((<|>), many)

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Bool Bool deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%|*=-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

notDigit :: Parser Char
notDigit = satisfy $ not <$> isDigit

parseString :: Parser Value
parseString = String <$> (char '"' *> (manyTill anyChar $ try $ char '"'))

parseNumber :: Parser Value
parseNumber = Number <$> read <$> many1 digit <* notFollowedBy (alphaNum <|> symbol)

parseBool :: Parser Value
parseBool =  string "#t" *> pure (Bool True)
         <|> string "#f" *> pure (Bool False)

parseAtom :: Parser Value
parseAtom = Atom <$> (notFollowedBy (digit) *> many (letter <|> digit <|> symbol))

parseExpr :: Parser Value
parseExpr =  char '(' *>
                try parseDottedList <|> parseList
             <* char ')'
         <|> parseNumber
         <|> parseString
         <|> parseBool
         <|> parseQuoted
         <|> parseAtom

parseList :: Parser Value
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser Value
parseDottedList = DottedList <$> (endBy parseExpr spaces) <*> (char '.' *> spaces *> parseExpr)    

parseQuoted :: Parser Value
parseQuoted = List <$> ((Atom "quote") :) <$> (: []) <$> (char '\'' *> parseExpr)

main = getLine >>= parseTest parseExpr
  



