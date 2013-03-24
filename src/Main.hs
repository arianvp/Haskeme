module Main
( main
) where
import Language.Haskeme.AST
import Language.Haskeme.Parse
import Text.Parsec (parseTest)
import System.Environment

main = do c <- getArgs
          parseTest parser (c!!0)
