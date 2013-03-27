module Main
( main
) where

import           Language.Haskeme.Eval  (eval)
import           Language.Haskeme.Error
import           System.Environment     (getArgs)



main :: IO ()
main = (either print print) . eval . head =<< getArgs
