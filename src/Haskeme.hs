module Main(main) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String
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
notDigit = satisfy $ not . isDigit

parseString :: Parser Value
parseString = String <$> (char '"' *> (manyTill anyChar $ try $ char '"'))

parseNumber :: Parser Value
parseNumber = Number <$> (read <$> many1 digit <* (notFollowedBy notDigit))

parseBool :: Parser Value
parseBool =  string "#t" *> pure (Bool True)
         <|> string "#f" *> pure (Bool False)

parseAtom :: Parser Value
parseAtom = Atom <$> ((notFollowedBy digit) *> many (letter <|> digit <|> symbol))


main = do putStrLn "string:"
          getLine >>= parseTest parseString
          putStrLn "number:"
          getLine >>= parseTest parseNumber
          putStrLn "bool:"
          getLine >>= parseTest parseBool
          putStrLn "atom:"
          getLine >>= parseTest parseAtom

          
