module Haskeme(main) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Control.Applicative

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%|*=-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser Value
parseString = String <$> (char '"' *> (manyTill anyChar $ try $ char '"'))
