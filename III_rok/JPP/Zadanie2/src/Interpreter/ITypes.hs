module ITypes where
import AbsGrammar

import Data.Map as M
import Data.List (intercalate)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


type MM = ReaderT MyEnv (StateT MyStore (ExceptT RuntimeExceptions IO))

type Loc = Int

type MyState = M.Map Loc ValueType

type MyStore = (MyState, Loc)

type MyEnv = M.Map Ident Loc

type ListDecl = (Type, [ValueType])

type FunDecl = (Type, MyEnv, [Arg], [Stmt])

type ReturnValue = Maybe ValueType

data ValueType = VInt Integer |
                VStr String |
                VBool Bool |
                VFun FunDecl |
                VList ListDecl |
                FBreak |
                FContinue |
                VNone
                deriving (Eq, Ord, Read)

instance Show ValueType where
    show (VInt i)       = show i
    show (VStr s)       = s
    show (VBool b)      = show b
    show (VFun f)       = "VFun " ++ show f
    show (VList (_, l)) = "[" ++ intercalate ", " (Prelude.map show l) ++ "]"
    show _              = undefined -- VNone, Break and Continue are internal values


data RuntimeExceptions =    DivisionByZeroException Expr |
                            PopOnEmptyList |
                            NoReturnException Type Expr [Arg] |
                            OutOfRangeExeption Integer |
                            NoMainFunctionException |
                            MainFunctionArgsException

instance Show RuntimeExceptions where
    show (DivisionByZeroException expr) = "RuntimeException: Division by zero " ++
        "in ’" ++ show expr ++ "’" 
    
    show PopOnEmptyList = "RuntimeException: Attempted to pop on empty list"
    
    show (NoReturnException fType e fArgs) =
        "RuntimeException: No return statement in function " ++
        "’" ++ show fType ++ " " ++ show e ++ "(" ++ show fArgs ++ ")’"
    
    show (OutOfRangeExeption i) =
        "RuntimeException: Index " ++ show i ++ " is out of range"
    
    show NoMainFunctionException = "RuntimeException: Main function not found"
    show MainFunctionArgsException =
        "RuntimeException: Main function should not have arguments"