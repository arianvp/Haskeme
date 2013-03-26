module Language.Haskeme.Error
( throwError
, SchemeError (..)
) where
import Text.Parsec.Error (ParseError)
import Control.Monad.Error 
import Language.Haskeme.AST (Expr, unwords')


data SchemeError = Parser ParseError
                 | TypeMismatch String Expr
                 | NumArgs Integer [Expr]
                 | BadSpecialForm String Expr
                 | NotFunction String String
                 | UnboundVariable String String
                 | Default String


instance Error SchemeError where
    noMsg = Default "An error has occured"
    strMsg = Default

instance Show SchemeError where
    show (UnboundVariable message variable) = message ++ ": " ++ variable
    show (BadSpecialForm message form)      = message ++ ": " ++ show form
    show (NotFunction message function)     = message ++ ": " ++ show function
    show (NumArgs expected found)           = "Expected " ++ show expected
                                                ++ " arguments; found values " ++ unwords' found
    show (TypeMismatch expected found)      = "Invalid type: expected " ++ expected ++ ", found "
                                                ++ show found
    show (Parser error)                     = "Parse error at " ++ show error
    show (Default msg)                      = msg

type ThrowsError = Either SchemeError

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

