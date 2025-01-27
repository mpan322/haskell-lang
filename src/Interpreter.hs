module Interpreter where
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Control.Monad.Except
import Control.Applicative
import Data.Maybe
import Parser
import Error

-- frame has local variable and local function table
--
type Frame = M.Map String Data

initFrame :: Frame
initFrame = M.empty

initState :: EvalState
initState = EvalState { 
    globals = initFrame,
    funcs   = M.empty,
    env     = (initFrame, []),
    store   = [] }

evaluate :: [ASTStmt] -> IO (Either Error ())
evaluate prog = evalStateT (runExceptT $ eval prog) initState

data EvalState = EvalState {
    globals :: Frame,
    funcs :: M.Map String ([String], [ASTStmt]),
    env :: (Frame, [Frame]),
    store :: [(Frame, [Frame])]
}

type Eval a = ExceptT Error (StateT EvalState IO) a

pushFrame :: Eval ()
pushFrame = do
    (f, fs) <- gets env
    modify $ \s -> s { env = (initFrame, f:fs) }

popFrame :: Eval ()
popFrame = do
    (f, fs) <- gets env
    case fs of
        []   -> throwError $ BadImpl "pop from singleton environment"
        r:rs -> modify $ \s -> s { env = (r, rs) }

startEnv :: Eval ()
startEnv = do
    e <- gets env
    sto <- gets store
    let ne = (initFrame, [])
    modify $ \s -> s { env = ne, store = e:sto }

closeEnv :: Eval ()
closeEnv = do
    sto <- gets store
    case sto of
        []   -> throwError $ BadImpl "restore when no env"
        r:rs -> modify $ \s -> s { env = r, store = rs }

getFrame :: Eval Frame
getFrame = gets (fst.env)

getEnv :: Eval (Frame, [Frame])
getEnv = gets env

modifyFrame :: (Frame -> Frame) -> Eval ()
modifyFrame h = do
    (f, fs) <- getEnv
    modify (\s -> s { env = (h f, fs) })

modifyStack :: ([Frame] -> [Frame]) -> Eval ()
modifyStack h = do
    (f, fs) <- getEnv
    modify  $ \s -> s { env = (f, h fs) }

foldUntil :: (Alternative t, Foldable f) => (a -> t b) -> f a -> t b
foldUntil f vs = foldr (\a b -> b <|> f a) empty vs

getVar :: String -> Eval Data
getVar var = do
    (f, fs) <- getEnv
    gs <- gets globals
    let m = M.lookup var f <|> _getVar var fs <|> M.lookup var gs
    case m of
        Nothing -> throwError $ NoSuchVar var
        Just da -> return da
    where
        _getVar :: String -> [Frame] -> Maybe Data
        _getVar var fs = foldUntil (M.lookup var) fs

getFunc :: String -> Eval ([String], [ASTStmt])
getFunc n = do
    fns <- gets funcs
    case M.lookup n fns of
        Just v  -> return v
        Nothing -> throwError $ NoSuchFunc n

getMain :: Eval [ASTStmt]
getMain = do
    fns <- gets funcs
    case M.lookup "main" fns of
        Just (_, b) -> return b
        Nothing     -> throwError NoMainFunc

bindVar :: String -> Data -> Eval ()
bindVar var dat = do
    f <- getFrame
    if M.member var f then throwError $ DuplicateBinding var
    else modifyFrame (\f -> M.insert var dat f)

bindGlobal :: String -> Data -> Eval ()
bindGlobal var dat = do
    gs <- gets globals
    if M.member var gs then throwError $ DuplicateBinding var
    else modify $ \s -> s { globals = M.insert var dat gs }

bindFunc :: String -> [String] -> [ASTStmt] -> Eval ()
bindFunc name params body = do
    fs <- gets funcs
    if M.member name fs then throwError $ DuplicateBinding name
    else modify $ \s -> s { funcs = M.insert name (params, body) fs }

setVar :: String -> Data -> Eval ()
setVar var dat = do
    (f, fs) <- getEnv
    if M.member var f then modifyFrame (\f -> M.insert var dat f)
    else do
        fs' <- helper var dat fs
        modifyStack (\_ -> fs')
    where
        helper :: String -> Data -> [Frame] -> Eval [Frame]
        helper var _ []       = throwError $ NoSuchVar var
        helper var dat (f:fs) =
            if M.member var f then 
                let f' = M.insert var dat f in
                return (f':fs)
            else do
                fs' <- helper var dat fs
                return (f:fs')

evalGlobals :: [ASTStmt] -> Eval ()
evalGlobals stmts = forM_ stmts helper
    where
        helper :: ASTStmt -> Eval ()
        helper (LetStmt n e) = do
            v <- evalExpr e
            bindGlobal n v
        helper (FunStmt n ps bdy) = bindFunc n ps bdy
        helper _ = throwError IllegalGlobal

findMain :: [ASTStmt] -> Eval ASTStmt
findMain (f@(FunStmt n _ _):xs) = 
    if n == "main" then return f
    else findMain xs
findMain [] = throwError NoMainFunc

eval :: [ASTStmt] -> Eval ()
eval stmts = do
    evalGlobals stmts
    body <- getMain
    evalBody body
    return ()

evalCall :: String -> [ASTExpr] -> Eval Data
evalCall "print" params = do
    vprms <- forM params evalExpr
    liftIO $ putStrLn (show vprms)
    return Void
evalCall name params = do
    vparams <- mapM evalExpr params
    (nparams, bdy) <- getFunc name
    startEnv
    -- bind parameters in new environment
    forM_ (zip nparams vparams) (uncurry bindVar)
    ret <- evalBody bdy
    closeEnv
    return (fromMaybe Void ret)

dumpEnv :: Eval ()
dumpEnv = do
    env <- gets env
    liftIO $ putStrLn (show env)

evalBody :: [ASTStmt] -> Eval (Maybe Data)
evalBody []     = return Nothing
evalBody (x:xs) =
    case x of
        LetStmt n e  -> do
            v <- evalExpr e
            bindVar n v
            evalBody xs
        AssignStmt n e -> do
            v <- evalExpr e
            setVar n v
            evalBody xs
        RetStmt e    -> Just <$> evalExpr e
        CallStmt n p -> do
            evalCall n p
            evalBody xs
        IfStmt c ifs els -> do
            v <- evalExpr c
            case v of
                Bool True -> do
                    pushFrame
                    r <- evalBody ifs
                    popFrame
                    if isNothing r then evalBody xs
                    else return r
                Bool False -> do
                    pushFrame
                    r <- evalBody els
                    popFrame
                    if isNothing r then evalBody xs
                    else return r
                _ -> throwError $ BadValue "if conditions must be booleans"
        _ -> throwError $ BadImpl "statement not implemented"

binary :: (a -> a -> b) -> (Data -> Eval a) -> (b -> Data) -> ASTExpr -> ASTExpr -> Eval Data
binary f unwrap cons l r = do
    lv <- evalExpr l >>= unwrap
    rv <- evalExpr r >>= unwrap
    return $ cons $ f lv rv

binaryCmp :: (Int -> Int -> Bool) -> ASTExpr -> ASTExpr -> Eval Data
binaryCmp f l r = binary f unwrapInt Bool l r

binaryInt :: (Int -> Int -> Int) -> ASTExpr -> ASTExpr -> Eval Data
binaryInt f l r = binary f unwrapInt Int l r

binaryBool :: (Bool -> Bool -> Bool) -> ASTExpr -> ASTExpr -> Eval Data
binaryBool f l r = binary f unwrapBool Bool l r

unwrapInt :: Data -> Eval Int
unwrapInt (Int v) = return v
unwrapInt _ = throwError $ BadImpl "int expected"

unwrapBool :: Data -> Eval Bool
unwrapBool (Bool v) = return v
unwrapBool _ = throwError $ BadImpl "bool expected"

evalExpr :: ASTExpr -> Eval Data
evalExpr (Add l r) = binaryInt (+) l r
evalExpr (Sub l r) = binaryInt (-) l r
evalExpr (Eq l r) = binaryCmp (==) l r
evalExpr (Lt l r) = binaryCmp (<) l r
evalExpr (Gt l r) = binaryCmp (>) l r
evalExpr (And l r) = binaryBool (&&) l r
evalExpr (Or l r) = binaryBool (||) l r
evalExpr (ConstExpr v) = return v
evalExpr (CallExpr n ps) = evalCall n ps
evalExpr (VarExpr n) = getVar n





