module Main
( main
) where

import           Language.Haskeme.Eval  (eval)
import           System.Environment     (getArgs)



main :: IO ()
main = print . eval . head =<< getArgs


