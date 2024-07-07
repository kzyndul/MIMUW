module Interpreter where

import AbsGrammar
import ITypes
import IUtils
import Eval

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import System.IO (hPutStrLn, stderr)




import Data.Map as M


data RuntimeError = RuntimeError String deriving (Show)


prepareEnv :: [Fn] -> MM (MyEnv, MyStore)
prepareEnv ((FnDef typ ident args (Block stmts)):fs) = do
    newEnv <- declareVar ident VNone
    local (const newEnv) (putValueToMemory ident (VFun (typ, newEnv, args, stmts)))
    local (const newEnv) (prepareEnv fs)
prepareEnv _ = do
    env <- ask
    store <- get
    return (env, store)



initialEnv :: MyEnv
initialEnv = M.empty

initialStore :: MyStore
initialStore = (M.empty, 0)



checkMainFunctionArgs :: Loc -> MyStore -> Either RuntimeExceptions ()
checkMainFunctionArgs loc (myState, _) =
    case M.lookup loc myState of
        Just (VFun (_, _, args, _)) ->
            if Prelude.null args
                then return ()
                else Left MainFunctionArgsException
        _ -> Left NoMainFunctionException

handleResult :: Either RuntimeExceptions ValueType -> IO ()
handleResult result = case result of
    Left err -> hPutStrLn stderr $ show err
    Right _ -> return ()

runMainFunction :: MyEnv -> MyStore -> IO ()
runMainFunction env store = do
    result <- runExceptT (runStateT (runReaderT (evalExpr (EApp (EVar (Ident "main")) [])) env) store)
    handleResult (fmap fst result)

checkMainAndRun :: MyEnv -> MyStore -> Loc -> IO ()
checkMainAndRun env store loc = case checkMainFunctionArgs loc store of
    Left err -> hPutStrLn stderr $ show err
    Right _ -> runMainFunction env store

runMainIfPresent :: MyEnv -> MyStore -> IO ()
runMainIfPresent env store = case M.lookup (Ident "main") env of
    Nothing -> hPutStrLn stderr $ "RuntimeException: No main function found"
    Just loc -> checkMainAndRun env store loc

runProgram :: Program -> IO ()
runProgram (PProgram program) = do
    result <- runExceptT (runStateT (runReaderT (prepareEnv program) initialEnv) initialStore)
    case result of
        Left err -> hPutStrLn stderr $ show err
        Right ((env, store), _) -> runMainIfPresent env store
