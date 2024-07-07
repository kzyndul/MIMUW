module IUtils where
import AbsGrammar
import ITypes

import Control.Monad.Reader
import Control.Monad.State

import Data.Map as M
import Data.Maybe



putValueToMemory :: Ident -> ValueType -> MM ()
putValueToMemory ident val = do
    env <- ask
    (store, loc) <- get
    put (M.insert (fromJust $ M.lookup ident env) val store, loc)
    return ()

putLocToMemory :: Ident -> Loc -> MM MyEnv
putLocToMemory ident loc = ask >>= \env -> return (M.insert ident loc env)



declareVar :: Ident -> ValueType -> MM MyEnv
declareVar ident val = do
    env <- ask
    (store, newLockNumber) <- get 
    let env2 = M.insert ident newLockNumber env
    put (M.insert newLockNumber val store, newLockNumber + 1)
    return env2



extractLocFromIdent :: Ident -> MM Loc
extractLocFromIdent name = ask >>= \env -> return (env M.! name)

extractValueFromIdent :: Ident -> MM ValueType
extractValueFromIdent ident = do
    loc <- extractLocFromIdent ident
    (store, _) <- get
    return $ store M.! loc 