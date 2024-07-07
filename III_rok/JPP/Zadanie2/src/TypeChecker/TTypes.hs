module TTypes where
import Control.Monad.Trans.Except
import Control.Monad.Reader
import qualified Data.Map as M
import AbsGrammar

data ExprOrStmt = Expr Expr | Stmt Stmt deriving Show

data TypeCheckExceptions = 
    ListConstructorError Expr String |
    UnexpectedRefException Type TypeOrRef |
    ExpectedRefException TypeOrRef Type |
    ExpectedListException ExprOrStmt Type |
    NonexistingIdentifierException Ident |
    UninitializedVariableException Expr |
    TypeMismatchException Type Type Expr |
    ExpectedFunctionException Type Expr |
    FuncApplicationException Type Expr [TypeOrRef] (Maybe String) |
    NotInsideLoopError Stmt

instance Show TypeCheckExceptions where
    show (ListConstructorError expr str) =
        "Error: Error while initializing list "++
        "’" ++ show expr ++ "’\n" ++ "additional information " ++ str
    
    show (UnexpectedRefException t e) = "Error: Unexpected reference. " ++
        "Expected type ’" ++ show t ++ "’ got ’" ++ show e ++ "’"
    
    show (ExpectedRefException t e) = "Error: Expected reference. " ++
        "Expected type ’" ++ show t ++ "’ got ’" ++ show e ++ "’"
    
    show (ExpectedListException e typ) = "Error: Expected list type in " ++
        "’" ++ show e ++ "’ actual type " ++ show typ ++ ""
    
    show (NonexistingIdentifierException name) = "Error: " ++
        "’" ++ show name ++ "’ was not declared in this scope"
    
    show (UninitializedVariableException expr) =
        "Error: Use of uninitialized variable " ++
        "’" ++ show expr ++ "’"
    
    show (TypeMismatchException typ1 typ2 expr) = "Error: Type error " ++
        "expected type ’" ++ show typ1 ++ "’" ++
        " actual type ’" ++ show typ2 ++ "’" ++
        " in ’" ++ show expr ++ "’"

    show (ExpectedFunctionException typ expr) =
        "Error: Expected function type in " ++
        "’" ++ show expr ++ "’ actual type" ++ show typ ++ ""
       
    show (FuncApplicationException functionType name fArg Nothing) =
        "Error: Invalid number of arguments applied to function " ++
        "‘" ++ show functionType ++ " " ++ show name ++ "(" ++ show fArg ++ ")’"

    show (FuncApplicationException functionType name fArg (Just str)) =
        "Error: Type error in function application " ++
        "‘" ++ show functionType ++ " " ++ show name ++ "(" ++ show fArg ++ ")’\n" ++
        "additional information: " ++ str

    show (NotInsideLoopError bc) = case bc of
        Break       -> "Error: Break outside of loop"
        Continue    -> "Error: Continue outside of loop"
        _           -> undefined
-- ‘a’

-- information if variable is initialized
type Initialized = Bool

data TCEnv = TCEnv {
  env                           :: M.Map Ident (Type, Initialized),
  currentFunctionExpectedValue  :: Type,
  insideLoop                    :: Bool
}

type TCM = ReaderT TCEnv (ExceptT TypeCheckExceptions IO)