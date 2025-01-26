module Interpreter where
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Control.Monad.Except
import Control.Applicative
import Parser

-- frame has local variable and local function table

type Code = [String]

data Data = Int Int
    | Func [String] [ASTStmt] Env
    | Void

instance Show Data where
    show = showType

showType :: Data -> String
showType (Func _ _ _) = "func"
showType Void = "void"
showType (Int _) = "int"

type Frame = M.Map String Data

initFrame :: Frame
initFrame = M.empty


data Env = Root Frame | Env Frame Env

callEnv :: Env -> Eval Env
callEnv e = do
    let e' = Env initFrame e
    modify $ \s -> s { env = e' }
    return e'

initEnv :: Env
initEnv = Root M.empty

data EvalError = UnknownVariable String
    | IncorrectType String
    | IncorrectNumberOfArgs
    | NoMainDefined
    | CannotCallValue
    | DuplicateBinding String
    | IllegalStatement String
    deriving Show

setFrame :: Frame -> Env -> Env
setFrame f (Root _) = Root f
setFrame f (Env _ p) = Env f p

getFrame :: Env -> Frame
getFrame (Root f) = f
getFrame (Env f _) = f

dumpEnv :: Eval ()
dumpEnv = do
    e <- getEnv
    helper e
    where
        helper :: Env -> Eval ()
        helper (Root f)  = liftIO $ putStrLn $ show f
        helper (Env f e) = do
            liftIO $ putStrLn "-----"
            liftIO $ putStrLn $ show f
            helper e

-- gets the value of a variable from the current environment
-- fails if the variable does not exist.
getVar :: String -> Eval Data
getVar name = do
    e <- getEnv
    case _getVar name e of
        Nothing -> do
            dumpEnv
            throwError $ UnknownVariable name
        Just va -> return va
    where
        _getVar :: String -> Env -> Maybe Data
        _getVar name (Root frame) = M.lookup name frame
        _getVar name (Env frame parent) = M.lookup name frame <|> _getVar name parent

getEnv :: Eval Env
getEnv = gets env

getRet :: Eval Data
getRet = do
    r <- gets ret
    setRet Void
    return r

-- binds a variable in the lowest frame of the current env
-- fails if the variable is already bound
bindVar :: String -> Data -> Eval ()
bindVar name val = do
    e <- getEnv
    case _bindVar name val e of
        Nothing -> throwError (DuplicateBinding name)
        Just e' -> setEnv e'
_bindVar :: String -> Data -> Env -> Maybe Env
_bindVar name val env = 
    let f = getFrame env in
    case M.lookup name f of
        Nothing -> do
            let f' = M.insert name val f
            return $ setFrame f' env
        _       -> Nothing

bindVarP :: String -> Data -> Env -> (Env, Bool)
bindVarP name val e = 
    case _bindVar name val e of
        Nothing -> (e, False)
        Just e' -> (e', True)

bindVars :: [String] -> [Data] -> Eval ()
bindVars ns vs = helper ns vs
    where
        helper :: [String] -> [Data] -> Eval ()
        helper (n:ns) (v:vs) = do
            bindVar n v
            helper ns vs
        helper [] [] = return ()
        helper _ _ = throwError IncorrectNumberOfArgs

setEnv :: Env -> Eval ()
setEnv e = modify $ \s -> s { env = e }

-- sets the value of a bound variable to 
-- a new value in the environment
-- fails if no variable exists.
setVar :: String -> Data -> Eval ()
setVar name val = do
    e <- getEnv
    case _setVar name val e of
        Nothing -> throwError $ UnknownVariable name
        Just e' -> setEnv e'
    where
        _setVar :: String -> Data -> Env -> Maybe Env
        _setVar name val (Root frame) = do
            M.lookup name frame
            let f = M.insert name val frame
            return $ Root f 
        _setVar name val (Env frame parent) = do
            M.lookup name frame
            let f = M.insert name val frame
            return $ Env f parent
            <|>
            _setVar name val parent

data EvalState = EvalState {
    env :: Env,
    ret :: Data
}

type Eval a = ExceptT EvalError (StateT EvalState IO) a

findMain :: [ASTStmt] -> Eval ASTStmt
findMain fs = 
    case inner fs of
        Nothing -> throwError NoMainDefined
        Just f  -> return f
    where
        inner :: [ASTStmt] -> Maybe ASTStmt
        inner [] = Nothing
        inner (f:fs) = helper f <|> inner fs

        helper :: ASTStmt -> Maybe ASTStmt
        helper f@(FunStmt name _ _) = 
            if name == "main" then return f
            else Nothing
        helper _ = Nothing

evaluate :: [ASTStmt] -> IO (Either EvalError ())
evaluate prog = evalStateT (runExceptT $ eval prog) initState
    where initState = EvalState { env = initEnv, ret = Void }

eval :: [ASTStmt] -> Eval ()
eval globals = do
    (FunStmt _ _ body) <- findMain globals
    evalDecls globals
    e <- getEnv
    evalCall [] body e []
    return ()

evalDecls :: [ASTStmt] -> Eval ()
evalDecls decls = fmap (\_ -> ()) $ mapM helper decls
    where
        helper :: ASTStmt -> Eval ()
        helper f@(FunStmt n p b) = bindFunc n p b
        helper _ = throwError (IllegalStatement "top level statements must be function declarations.")

bindFunc :: String -> [String] -> [ASTStmt] -> Eval ()
bindFunc n p b = do
    e <- getEnv
    let f = Func p b e'
        (e', t) = bindVarP n f e in 
        if t then setEnv e'
        else throwError $ DuplicateBinding n

isFunc :: Data -> Bool
isFunc (Func _ _ _) = True
isFunc _ = False

printExpr :: ASTExpr -> Eval ()
printExpr e = do
    r <- evalExpr e
    case r of
        Int v -> liftIO $ putStrLn (show v)
        _     -> throwError $ IncorrectType ("cannot print: " ++ showType r)

evalStmt :: ASTStmt -> Eval ()
evalStmt (FunStmt n p b) = bindFunc n p b
evalStmt (LetStmt n expr) = do
    v <- evalExpr expr
    bindVar n v
evalStmt (CallStmt n pvals) = do
    if n == "print" then do
        mapM printExpr pvals
        return ()
    else do
        f <- getVar n
        case f of
            Func p b e -> do
                evalCall p b e pvals
                setRet Void
                return ()
            _          -> throwError $ CannotCallValue
evalStmt (RetStmt expr) = do
    v <- evalExpr expr
    setRet v

setRet :: Data -> Eval ()
setRet d = modify $ \s -> s { ret = d }

evalBinary :: (Data -> Data -> Eval Data) -> ASTExpr -> ASTExpr -> Eval Data
evalBinary op l r = do
    lv <- evalExpr l
    rv <- evalExpr r
    op lv rv

liftInt :: (Int -> Int -> Int) -> (Data -> Data -> Eval Data)
liftInt op = \l r -> do
    lv <- unwrap l
    rv <- unwrap r
    return $ Int $ op lv rv
    where 
        unwrap :: Data -> Eval Int
        unwrap (Int v) = return v
        unwrap _ = throwError $ IncorrectType "int expected"

evalExpr :: ASTExpr -> Eval Data
evalExpr (Add l r) = evalBinary add l r
    where add = liftInt (+)
evalExpr (Sub l r) = evalBinary sub l r
    where sub = liftInt (-)
evalExpr (Mul l r) = evalBinary mul l r
    where mul = liftInt (*)
evalExpr (VarExpr v) = getVar v
evalExpr (ConstExpr v) = return $ Int $ read v
evalExpr (CallExpr n pvals) = do
    f <- getVar n
    case f of
        Func p b e ->  do
            evalCall p b e pvals
            getRet
        _          -> throwError $ CannotCallValue

evalCall :: [String] -> [ASTStmt] -> Env -> [ASTExpr] -> Eval Data
evalCall ps bdy e pexprs = do
    sto <- getEnv
    pvals <- mapM evalExpr pexprs
    callEnv e
    bindVars ps pvals
    evalBody bdy
    modify $ \s -> s { env = sto } -- restore the programme state
    return Void

evalBody :: [ASTStmt] -> Eval ()
evalBody [] = return ()
evalBody (x:xs) = case x of
    RetStmt _ -> evalStmt x
    _         -> do
        evalStmt x
        evalBody xs



--
-- loadTopLevel :: [ASTStmt] -> EvalST a

