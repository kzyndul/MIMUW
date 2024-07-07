module TypeChecker where



import AbsGrammar
import TTypes
import TUtils
import Control.Monad.Trans.Except
import Control.Monad.Except 
import Control.Monad.Reader
import qualified Data.Map as M

-- function check if the type of the expression is the same as the expected
-- type and if the variable is initialized. 
-- i => i' we only throw an error if expresion returns not initialized but we
-- expect it to be initialized.
expectedTypeExpr :: Type -> Initialized -> Expr -> TCM (Type, Initialized)
expectedTypeExpr expectedType expectedInitState e = do
    (actualType, actualInitState) <- typeCheckExpr e
    if (not actualInitState && expectedInitState)
        then throwError $ UninitializedVariableException e
        else if expectedType == actualType
            then return (expectedType, actualInitState || expectedInitState)
            else throwError $ TypeMismatchException expectedType actualType e



expectedTypeInitializedExpr :: Type -> Expr -> TCM (Type, Initialized)
expectedTypeInitializedExpr t e = expectedTypeExpr t True e

expectedTypeORRef :: TypeOrRef -> ExprOrRef -> TCM (Type, Initialized)
expectedTypeORRef (TypeOrRefType t) (ERExpr e) = expectedTypeInitializedExpr t e
expectedTypeORRef (TypeOrRefRef t) (ERRef r) = expectedTypeInitializedExpr t (EVar r)
expectedTypeORRef (TypeOrRefType t) (ERRef r) = do
    (actualType, _) <- getTypeFromEnv r
    throwError $ UnexpectedRefException t (TypeOrRefRef actualType)

expectedTypeORRef (TypeOrRefRef t) (ERExpr e) = do
    (actualType, _) <- expectedTypeInitializedExpr t e
    throwError $ ExpectedRefException (TypeOrRefRef t) actualType


-- function checks if e1 and e2 are initialized and have the same type
typeCheckERel :: Expr -> RelOp -> Expr -> TCM (Type, Initialized)
typeCheckERel e1 op e2 = do
    (t1, i1) <- typeCheckExpr e1
    (t2, i2) <- typeCheckExpr e2 
    if (not i1 || not i2)
        then if not i1
            then throwError $ UninitializedVariableException e1
            else throwError $ UninitializedVariableException e2
        else if t1 == t2
            then return (Bool, True)
            else throwError $ TypeMismatchException t1 t2 (ERel e1 op e2)


typeCheckExpr :: Expr -> TCM (Type, Initialized)
typeCheckExpr (EVar ident) = getTypeFromEnv ident
typeCheckExpr (ELitInt _) = return (Int, True)
typeCheckExpr (ELitTrue) = return (Bool, True)
typeCheckExpr (ELitFalse) = return (Bool, True)
typeCheckExpr (EString _) = return (Str, True)

-- under indent has to be list
typeCheckExpr (EListLength ident) = do
    (actualType, isInitialized) <- getTypeFromEnv ident
    case (actualType, isInitialized) of
        (TList _, True) -> return (Int, True)
        (TList _, False) -> throwError $ UninitializedVariableException (EListLength ident)
        _ -> throwError $ ExpectedListException (Expr (EListLength ident)) actualType 

-- all elements in list have to be of the same type as list type.
typeCheckExpr (EListEmpty (TList listType) initializationList) = do
    mapM_ checkExprType initializationList
    return (TList listType, True)
    where
        checkExprType :: Expr -> TCM (Type, Initialized)
        checkExprType e = expectedTypeInitializedExpr listType e `catchError`
            (\err -> throwError $
                ListConstructorError (EListEmpty listType initializationList)
                (show err))
typeCheckExpr (EListEmpty t initializationList) =
    throwError $ ExpectedListException (Expr (EListEmpty t initializationList)) t


-- under indent has to be list
typeCheckExpr (EListAt ident e) = do
    (actualType, isInitialized) <- getTypeFromEnv ident
    case (actualType, isInitialized) of
        (TList listType, True) -> expectedTypeInitializedExpr Int e >> return (listType, True)
        (TList _, False) -> throwError $ UninitializedVariableException (EListAt ident e)
        _ -> throwError $ ExpectedListException (Expr (EListAt ident e)) actualType 

typeCheckExpr (Neg e) = expectedTypeInitializedExpr Int e
typeCheckExpr (Not e) = expectedTypeInitializedExpr Bool e
typeCheckExpr (EAdd e1 _ e2) = expectedTypeInitializedExpr Int e1 >> expectedTypeInitializedExpr Int e2
typeCheckExpr (EMul e1 _ e2) = expectedTypeInitializedExpr Int e1 >> expectedTypeInitializedExpr Int e2

-- u can compere equal of all types
typeCheckExpr (ERel e1 EQU e2) = typeCheckERel e1 EQU e2
typeCheckExpr (ERel e1 NE e2) = typeCheckERel e1 NE e2
-- u can compere less or greater only ints
typeCheckExpr (ERel e1 _ e2) = expectedTypeInitializedExpr Int e1 >> expectedTypeInitializedExpr Int e2 >> return (Bool, True)

typeCheckExpr (EAnd e1 e2) = expectedTypeInitializedExpr Bool e1 >> expectedTypeInitializedExpr Bool e2
typeCheckExpr (EOr e1 e2) = expectedTypeInitializedExpr Bool e1 >> expectedTypeInitializedExpr Bool e2

typeCheckExpr (EApp e eArgs) = do
    (exprType, _) <- typeCheckExpr e
    case exprType of
        Fun funType fArgs -> do
            if (length fArgs) /= (length eArgs)
                then throwError $ FuncApplicationException funType e fArgs Nothing
                else do 
                    mapM_ checkArgType (zip fArgs eArgs) `catchError`
                        (\err -> throwError $
                            FuncApplicationException funType e fArgs (Just (show err)))
                    return (funType, True)
        
        _ -> throwError $ ExpectedFunctionException exprType (EApp e eArgs)
    where
        checkArgType :: (TypeOrRef, ExprOrRef) -> TCM (Type, Initialized)
        checkArgType (fArg, expr) = do 
            expectedTypeORRef fArg expr


typeCheckStmt :: Stmt -> TCM TCEnv
-- return not changed env
typeCheckStmt (BStmt (Block stmts)) = do
    currEnv <- ask
    _ <- local (const currEnv) (typeCheckMany stmts)
    return currEnv

typeCheckStmt (Ass ident e) = do
    (typ, _) <- getTypeFromEnv ident
    _ <- expectedTypeInitializedExpr typ e
    putTypeToEnv ident (typ, True)

typeCheckStmt (Ret expr) = do
    tcEnv <- ask
    let expectedType = currentFunctionExpectedValue tcEnv
    _ <- expectedTypeInitializedExpr expectedType expr
    return tcEnv

typeCheckStmt (Cond expr block) =
    expectedTypeInitializedExpr Bool expr >> typeCheckStmt (BStmt block)

typeCheckStmt (CondElse expr block elseBlock) = 
    expectedTypeInitializedExpr Bool expr >> 
    typeCheckStmt (BStmt block) >>
    typeCheckStmt (BStmt elseBlock)

typeCheckStmt (Break) = do
    currEnv <- ask
    unless (insideLoop currEnv) $ throwError (NotInsideLoopError Break)
    return currEnv
    
typeCheckStmt (Continue) = do
    currEnv <- ask
    unless (insideLoop currEnv) $ throwError (NotInsideLoopError Continue)
    return currEnv

typeCheckStmt (While expr block) = do
    _ <- expectedTypeInitializedExpr Bool expr
    local (\currEnv -> currEnv { insideLoop = True }) $ typeCheckStmt (BStmt block)

typeCheckStmt (Print expr) = typeCheckExpr expr >> ask 

typeCheckStmt (SExp expr) = typeCheckExpr expr >> ask

-- under indent has to be list
typeCheckStmt (ListPush ident expr) = do
  (actualType, isInitialized) <- getTypeFromEnv ident
  case (actualType, isInitialized) of
    (TList innerType, True) -> expectedTypeInitializedExpr innerType expr >> ask
    (TList _, False) -> throwError $ UninitializedVariableException (EVar ident)
    _ -> throwError $ ExpectedListException (Stmt (ListPush ident expr)) actualType

-- under indent has to be list
typeCheckStmt (ListPop ident) = do
  (actualType, isInitialized) <- getTypeFromEnv ident
  case (actualType, isInitialized) of
    (TList _, True) -> ask
    (TList _, False) -> throwError $ UninitializedVariableException (EVar ident)
    _ -> throwError $ ExpectedListException (Stmt (ListPop ident)) actualType

typeCheckStmt (Decl itemType items) = declTypeItems itemType items
    where
        declTypeItems :: Type -> [Item] -> TCM TCEnv
        declTypeItems _ (item:restItems) = do
            currEnv <- declItem itemType item
            local (const currEnv) (declTypeItems itemType restItems)
        declTypeItems _ [] = ask

        declItem :: Type -> Item -> TCM TCEnv
        declItem _ (Init ident expr) = do
            _ <- expectedTypeInitializedExpr itemType expr
            putTypeToEnv ident (itemType, True)
        declItem _ (NoInit ident) = putTypeToEnv ident (itemType, False)

typeCheckStmt (FnDecl (FnDef typ ident args (Block stmts))) = do
    fArgs <- mapM argToTypeOrRef args
    envWithFnDef <- putTypeToEnv ident (Fun typ fArgs, True)
    let newEnv = envWithFnDef { currentFunctionExpectedValue = typ }
    envWithArgs <- local (const newEnv) (declArgs args)
    _ <- local (const envWithArgs) (typeCheckMany stmts)
    return envWithFnDef
    where
        argToTypeOrRef :: Arg -> TCM TypeOrRef
        argToTypeOrRef (AArg t _) = return $ TypeOrRefType t
        argToTypeOrRef (RefArg t _) = return $ TypeOrRefRef t

        declArgs :: [Arg] -> TCM TCEnv
        declArgs (arg:restArgs) = do
            currEnv <- declArg arg
            local (const currEnv) (declArgs restArgs)
        declArgs [] = ask

        declArg :: Arg -> TCM TCEnv
        declArg (AArg t argIdent) = putTypeToEnv argIdent (t, True)
        declArg (RefArg t argIdent) = putTypeToEnv argIdent (t, True)


typeCheckMany :: [Stmt] -> TCM TCEnv
typeCheckMany (s:stmts) = do
    currEnv <- typeCheckStmt s
    local (const currEnv) (typeCheckMany stmts)
typeCheckMany [] = ask

typeCheckProgram :: [Fn] -> TCM TCEnv
typeCheckProgram (fn:fns) = do
    currEnv <- typeCheckStmt (FnDecl fn)
    local (const currEnv) (typeCheckProgram fns)
typeCheckProgram [] = ask


-- we initialize the environment with an empty map,
-- the expected value of the current function is Int (dose not matter),
-- and we are not inside a loop. 
initialTCEnv :: TCEnv
initialTCEnv = TCEnv M.empty Int False

runTypeCheck :: Program -> IO (Either TypeCheckExceptions TCEnv)
runTypeCheck (PProgram fns) = runExceptT (runReaderT (typeCheckProgram fns) initialTCEnv)