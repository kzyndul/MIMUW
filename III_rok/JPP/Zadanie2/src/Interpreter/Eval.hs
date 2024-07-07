{-# LANGUAGE TupleSections #-}

module Eval where

import AbsGrammar
import ITypes
import IUtils

import Control.Monad.Reader
import Control.Monad.Except

import Data.Maybe


getAddOp :: AddOp -> Integer -> Integer -> Integer
getAddOp Plus  = (+)
getAddOp Minus = (-)

evalAddOp :: AddOp -> ValueType -> ValueType -> ValueType
evalAddOp op (VInt v1) (VInt v2) = VInt $ getAddOp op v1 v2
evalAddOp _ _ _ = undefined -- supports only integer



evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp Times = (*)
evalMulOp Div   = (div)



-- uzgadniam wszystkie typy poniewaz typechecker sprawdza poprawnosc
evalRelOp :: RelOp -> ValueType -> ValueType -> ValueType
evalRelOp op v1 v2 = VBool $ getOp op v1 v2
    where
        getOp LTH = (<)
        getOp LE  = (<=)
        getOp GTH = (>)
        getOp GE  = (>=)
        getOp EQU = (==)
        getOp NE  = (/=)



passCurrentEnv :: MM (MyEnv, ReturnValue)
passCurrentEnv = fmap (, Nothing) ask



evalExpr :: Expr -> MM ValueType
evalExpr (EVar ident) = extractValueFromIdent ident


evalExpr (EString str) = return (VStr str)


evalExpr (ELitInt n) = return (VInt n)

evalExpr (Neg expr) = fmap negateInt (evalExpr expr)
    where
        negateInt (VInt n) = VInt (-n)
        negateInt _ = undefined

evalExpr (EAdd e1 op e2) = liftM2 (evalAddOp op) (evalExpr e1) (evalExpr e2)

evalExpr (EMul e1 op e2) = do
    VInt v1 <- evalExpr e1
    VInt v2 <- evalExpr e2
    case op of
        Div -> if v2 == 0
            then throwError $ DivisionByZeroException (EMul e1 op e2)
            else return $ VInt $ (evalMulOp op) v1 v2
        _   -> return $ VInt $ (evalMulOp op) v1 v2


evalExpr ELitTrue = return (VBool True)

evalExpr ELitFalse = return (VBool False)

evalExpr (Not expr) = fmap botBool (evalExpr expr)
    where
        botBool (VBool b) = VBool (not b)
        botBool _ = undefined

evalExpr (ERel e1 op e2) = liftM2 (evalRelOp op) (evalExpr e1) (evalExpr e2)

evalExpr (EAnd e1 e2) = do
    VBool b1 <- evalExpr e1
    if b1
        then evalExpr e2
        else return (VBool False)

evalExpr (EOr e1 e2) = do
    VBool b1 <- evalExpr e1
    if b1
        then return (VBool True)
        else evalExpr e2


evalExpr (EListEmpty listType expr) = do
    listVal <- mapM evalExpr expr
    return $ VList (listType, listVal)
    
evalExpr (EListLength ident) = do
    VList (_, elems) <- extractValueFromIdent ident
    return $ VInt $ fromIntegral $ length elems

evalExpr (EListAt ident expr) = do
    VInt index <- evalExpr expr
    VList (_, elems) <- extractValueFromIdent ident
    if index < 0 || index >= (fromIntegral $ length elems)
        then throwError $ OutOfRangeExeption index
        else return $ elems !! fromIntegral index

evalExpr (EApp ident args) = fmap (fromJust . snd) (runFunc ident args)


evalStmt :: Stmt -> MM (MyEnv, ReturnValue)
evalStmt (Decl _ items) = fmap (,Nothing) (declItems items) 
    where
        declItem :: Item -> MM MyEnv
        declItem (Init ident expr) = evalExpr expr >>= declareVar ident
        declItem (NoInit ident) = declareVar ident VNone

        declItems :: [Item] -> MM MyEnv
        declItems (item:restItems) = do
            newEnv <- declItem item
            local (const newEnv) (declItems restItems)
        declItems [] = ask

evalStmt (FnDecl (FnDef typ ident args (Block stmts))) = do
    newEnv <- declareVar ident VNone
    local (const newEnv) (putValueToMemory ident (VFun (typ, newEnv, args, stmts)))
    return (newEnv, Nothing)


evalStmt (Ret expr) = do
    ret <- evalExpr expr
    env <- ask
    return (env, Just ret)


evalStmt (ListPush ident expr) = do
    newHead <- evalExpr expr
    VList (typ, list) <- extractValueFromIdent ident
    putValueToMemory ident (VList (typ, (newHead:list)))
    passCurrentEnv

evalStmt (ListPop ident) = do
    VList (typ, list) <- extractValueFromIdent ident
    case list of
        (_:xs) -> putValueToMemory ident (VList (typ, xs)) >> passCurrentEnv
        [] -> throwError PopOnEmptyList


evalStmt (Cond expr block) = do
    VBool cond <- evalExpr expr
    if cond
        then evalStmt (BStmt block)
        else passCurrentEnv

evalStmt (CondElse expr block elseBlock) = do
    VBool cond <- evalExpr expr
    if cond
        then evalStmt (BStmt block)
        else evalStmt (BStmt elseBlock)


evalStmt (While expr (Block stmts)) = do
        VBool res <- evalExpr expr
        if res
            then do
                (env, ret) <- interpretMany stmts 
                if isNothing ret
                    then evalStmt (While expr (Block stmts))
                    else
                        case fromJust ret of
                            FBreak      -> return (env, Nothing)
                            FContinue   -> evalStmt (While expr (Block stmts))
                            _           -> return (env, ret)
            else 
                passCurrentEnv

evalStmt Break = fmap (, Just FBreak) ask 

evalStmt Continue = fmap (, Just FContinue) ask


evalStmt (BStmt (Block stmts)) = do
        env <- ask
        (_, ret) <- interpretMany stmts
        return (env, ret)


evalStmt (SExp e) = evalExpr e >> passCurrentEnv

evalStmt (Ass ident e) = evalExpr e >>= putValueToMemory ident >> passCurrentEnv

evalStmt (Print e) = do
        res <- evalExpr e
        liftIO $ putStr $ show res
        passCurrentEnv



interpretMany :: [Stmt] -> MM (MyEnv, ReturnValue)
interpretMany (stmt:stmts) = do
    (env2, ret) <- evalStmt stmt
    if isJust ret
        then return (env2, ret)
        else local (const env2) (interpretMany stmts)
interpretMany [] = fmap (, Nothing) ask


-- [ExprOrRef] list of passed arguments
-- [Arg] list of varablie names to map to passed arguments
-- appFunEnv env of function application required to calculate expr values
applyArgsToEnv :: [ExprOrRef] -> [Arg] -> MyEnv -> MM MyEnv
applyArgsToEnv ((ERRef passedName):eArgs) ((RefArg _typ name):fArgs) appFunEnv = do
    loc <- local (const appFunEnv) (extractLocFromIdent passedName)
    newEnv <- putLocToMemory name loc
    local (const newEnv) (applyArgsToEnv eArgs fArgs appFunEnv)
applyArgsToEnv ((ERExpr expr):eArgs) ((AArg _typ name):fArgs) appFunEnv = do
    val <- local (const appFunEnv) (evalExpr expr)
    newEnv <- declareVar name val
    local (const newEnv) (applyArgsToEnv eArgs fArgs appFunEnv)
applyArgsToEnv _ _ _ = ask



runFunc :: Expr -> [ExprOrRef] -> MM (MyEnv, ReturnValue)
runFunc expr vArgs = do
    curEnv <- ask
    VFun (retType, env, funArgs, stmts) <- evalExpr expr
    functionEnv <- local (const env) (applyArgsToEnv vArgs funArgs curEnv)
    (_, ret) <- local (const functionEnv) (interpretMany stmts)
    case ret of
        Just _    -> return (curEnv, ret)
        Nothing     -> throwError $ NoReturnException retType expr funArgs