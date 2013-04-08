module Language.Haskeme.Eval
( eval
) where
import           Control.Monad.Error
import           Language.Haskeme.AST (Expr (..))
import           Language.Haskeme.Parse
import           Language.Haskeme.Error (SchemeError (..))
import           Control.Applicative ((<$>))

-- a -> f b
eval :: String -> Either SchemeError Expr
eval input = (parse input >>= eval')

-- b -> f b
eval' :: Expr -> Either SchemeError Expr
eval' val@(String  _)            = Right val
eval' val@(Integer _)            = Right val
eval' val@(Bool    _)            = Right val
eval' (List [Atom "quote", val]) = Right val
eval' (List [Atom "if", pred, conseq, alt]) =
    do result <- eval' pred
       case result of
            Bool False -> eval' alt
            otherwise  -> eval' conseq
eval' (List ((Atom func):args))  = mapM eval' args >>= apply func
eval' val                        = Left $ Default $ "Cannot evaluate: " ++ show val

apply :: String -> [Expr] -> Either SchemeError Expr
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [Expr] -> Either SchemeError Expr)]
primitives = [("+"          , numOp (+))
             ,("-"          , numOp (-))
             ,("*"          , numOp (*))
             ,("/"          , numOp div)
             ,("mod"        , numOp mod)
             ,("quotient"   , numOp quot)
             ,("remainder"  , numOp rem)
             ,("="          , numBoolOp (==))
             ,("<"          , numBoolOp (<))
             ,(">"          , numBoolOp (>))
             ,("/="         , numBoolOp (/=))
             ,(">="         , numBoolOp (>=))
             ,("<="         , numBoolOp (<=))
             ,("and"        , boolBoolOp (&&))
             ,("or"         , boolBoolOp (||))
             ,("string=?"   , strBoolOp (==))
             ,("string<?"   , strBoolOp (<))
             ,("string>?"   , strBoolOp (>))
             ,("string<=?"  , strBoolOp (<=))
             ,("string>=?"  , strBoolOp (>=))]



numOp :: (Integer -> Integer -> Integer) -> [Expr] -> Either SchemeError Expr
numOp _ []      = throwError $ NumArgs 2 [(Atom "none")]
numOp _ one@[_] = throwError $ NumArgs 2 one
numOp op params = mapM unpackInt params >>= return . Integer . foldl1 op

boolOp :: (Expr -> Either SchemeError a) -> (a -> a -> Bool) -> [Expr] -> Either SchemeError Expr
boolOp _ _ []             = throwError $ NumArgs 2 [(Atom "none")]
boolOp _ _ one@[_]        = throwError $ NumArgs 2 one
boolOp unpack op params   = liftM (Bool . uncurry' op) $ mapM unpack params
                          where uncurry' f [x,y] = f x y


numBoolOp = boolOp unpackInt
strBoolOp = boolOp unpackString
boolBoolOp = boolOp unpackBool

unpackInt :: Expr -> Either SchemeError Integer
unpackInt (Integer n) = return n
unpackInt (List [n]) = unpackInt n
unpackInt notNum = throwError $ TypeMismatch "Integer" notNum

unpackBool :: Expr -> Either SchemeError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "Bool" notBool

unpackString :: Expr -> Either SchemeError String
unpackString (String s) = return s
unpackString notString  = throwError $ TypeMismatch "String" notString
