module Language.Haskeme.Eval
( eval
) where
import           Control.Monad.Error
import           Language.Haskeme.AST (Expr (..))
import           Language.Haskeme.Parse
import           Language.Haskeme.Error (SchemeError (..))


-- a -> f b
eval :: String -> Either SchemeError Expr
eval input = (parse input >>= eval')

-- b -> f b
eval' :: Expr -> Either SchemeError Expr
eval' val@(String  _)            = Right val
eval' val@(Integer _)            = Right val
eval' val@(Bool    _)            = Right val
eval' (List [Atom "quote", val]) = Right val
eval' val                        = Left $ Default $ show val
{-- eval' (List (Atom func:args))    = apply func $ map eval' args


apply :: String -> [Expr] -> Expr
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [Expr] -> Expr)]
primitives = [("+"          , numericBinOp (+))
             ,("-"          , numericBinOp (-))
             ,("*"          , numericBinOp (*))
             ,("/"          , numericBinOp div)
             ,("mod"        , numericBinOp mod)
             ,("quotient"   , numericBinOp quot)
             ,("remainder"  , numericBinOp rem)]

numericBinOp :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinOp op params = Integer $ foldl1 op $ map unpackInt params

unpackInt :: Expr -> Integer
unpackInt (Integer n) = n
--}
