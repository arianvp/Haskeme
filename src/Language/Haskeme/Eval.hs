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
eval' (List $ (Atom func):args)  = mapM eval' args >>= apply func
eval' val                        = Left $ Default $ show val

apply :: String -> [Expr] -> Either SchemeError Expr
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [Expr] -> Either SchemeError Expr)]
primitives = [("+"          , numericBinOp (+))
             ,("-"          , numericBinOp (-))
             ,("*"          , numericBinOp (*))
             ,("/"          , numericBinOp div)
             ,("mod"        , numericBinOp mod)
             ,("quotient"   , numericBinOp quot)
             ,("remainder"  , numericBinOp rem)]

numericBinOp :: (Integer -> Integer -> Integer) -> [Expr] -> Either SchemeError Expr
numericBinOp op one@[_] = throwError $ NumArgs 2 one
numericBinOp op params  = (mapM unpackInt params) >>= return . Integer . foldl1 op
unpackInt :: Expr -> Either SchemeError Integer
unpackInt (Integer n) = return n
unpackInt (List [n]) = unpackInt n
unpackInt notNum = throwError $ TypeMismatch "Integer" notNum

