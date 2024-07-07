module TUtils where
import AbsGrammar
import TTypes
import Control.Monad.Except 
import Control.Monad.Reader
import qualified Data.Map as M

getTypeFromEnv :: Ident -> TCM (Type, Initialized)
getTypeFromEnv ident =
    ask >>= \tcEnv -> case M.lookup ident (env tcEnv) of
        Just (t, i) -> return (t, i)
        Nothing     -> throwError $ NonexistingIdentifierException ident

putTypeToEnv :: Ident -> (Type, Initialized) -> TCM TCEnv
putTypeToEnv ident t =
    ask >>= \tcEnv -> return tcEnv { env = M.insert ident t (env tcEnv) }
