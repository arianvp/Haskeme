module Main
( main
) where
import Language.Haskeme.AST
import Language.Haskeme.Parse
import Text.Parsec (parseTest)

main = getLine >>= parseTest parser
