module Main
( main
) where

import           Language.Haskeme.Eval  (eval)
import           System.Environment     (getArgs)
import  		 System.IO
import 			 Control.Monad  		(unless)

main :: IO ()
main =  repl


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pred prompt action = do 
 	result <- prompt
 	unless (pred result) $ 
		action result >> until_ pred prompt action


repl :: IO ()
repl = until_ (== "quit") (readPrompt "Haskeme>>> ") (either print print . eval)