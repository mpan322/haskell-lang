module Parser where
import Control.Applicative
import qualified Lexer as L
import Control.Monad.Except
import Debug.Trace
import Error

data Data = Int Int
    | Void
    | Bool Bool

instance Show Data where
    show Void = "void"
    show (Int i) = show i
    show (Bool b) = show b

data Parser a = Parser { runParser :: [L.Token] -> Either Error ([L.Token], a) }

parse :: [L.Token] -> Either Error [ASTStmt]
parse toks = do
    (rst, out) <- runParser pprog toks 
    if rst /= [] then Left UnexpectedEOF
    else return out


instance Functor Parser where
    fmap f p = do
        v <- p
        return (f v)

instance Applicative Parser where
    pure v = Parser (\s -> Right (s, v))

    (<*>) p q = do
        f <- p
        v <- q
        return (f v)

instance Monad Parser where
    return = pure

    (>>=) p f = Parser $ \s -> do
        (s', v) <- runParser p s
        let q = f v
        runParser q s'

instance Alternative Parser where
    empty = Parser $ \_ -> Left $ BadImpl "parser empty"

    (<|>) p q = Parser $ \s -> 
        case runParser p s of
            Right r -> Right r
            Left _  -> runParser q s

next :: Parser L.Token
next = Parser helper
    where 
        helper []     = Left UnexpectedEOF
        helper (x:xs) = Right (xs, x)

sat :: (L.Token -> Bool) -> Parser L.Token
sat p = do
    v <- next
    if p v then return v
    else empty

eq :: L.Token -> Parser ()
eq tok = do
    sat (==tok)
    return ()


data ASTStmt = FunStmt String [String] [ASTStmt]
    | LetStmt String ASTExpr
    | AssignStmt String ASTExpr
    | RetStmt ASTExpr
    | IfStmt ASTExpr [ASTStmt] [ASTStmt]
    | CallStmt String [ASTExpr] deriving Show

data ASTExpr = VarExpr String
    | ConstExpr Data
    | Add ASTExpr ASTExpr
    | Mul ASTExpr ASTExpr
    | Sub ASTExpr ASTExpr
    | Eq ASTExpr ASTExpr
    | Lt ASTExpr ASTExpr
    | Gt ASTExpr ASTExpr
    | And ASTExpr ASTExpr
    | Or ASTExpr ASTExpr
    | Not ASTExpr
    | CallExpr String [ASTExpr] deriving Show

pprog :: Parser [ASTStmt]
pprog = many (pfunDecl <|> helper)
    where 
        helper = do
            l <- plet
            eq L.Semi
            return l

pfunDecl :: Parser ASTStmt
pfunDecl = do
    eq L.Fun
    name <- pident
    params <- pparams
    body <- pbody
    return $ FunStmt name params body

pident :: Parser String
pident = do
    ident <- next
    case ident of
        L.Ident n -> return n
        _         -> empty

pnum :: Parser String
pnum = do
    num <- next
    case num of
        L.Num n -> return n
        _       -> empty

pparams :: Parser [String]
pparams = do
    eq L.LParen
    eq L.RParen
    return []
    <|>
    do  eq L.LParen
        p  <- pident
        ps <- many $ do 
            eq L.Comma
            pident
        eq L.RParen
        return (p:ps)

pbody :: Parser [ASTStmt]
pbody = do
    eq L.LBrack
    stmts <- many $ do
        st <- pstatement 
        eq L.Semi
        return st
    eq L.RBrack
    return stmts

pstatement :: Parser ASTStmt
pstatement = do
    plet
    <|>
    preturn
    <|>
    pcallStmt
    <|>
    passign
    <|>
    pif

pif :: Parser ASTStmt
pif = do
    eq L.If
    eq L.LParen
    e <- pexpr
    eq L.RParen
    bif <- pbody
    eq L.Else
    belse <- pbody
    return $ IfStmt e bif belse


passign :: Parser ASTStmt
passign = do
    i <- pident
    eq L.Asgn
    e <- pexpr
    return (AssignStmt i e)

plet :: Parser ASTStmt
plet = do
    eq L.Let
    name <- pident
    eq L.Asgn
    e <- pexpr
    return (LetStmt name e)

preturn :: Parser ASTStmt
preturn = do
    eq L.Ret
    e <- pexpr <|> return (ConstExpr Void)
    return (RetStmt e)

pcallStmt :: Parser ASTStmt
pcallStmt = do
    name   <- pident
    params <- pparamExprs
    return (CallStmt name params)

pparamExprs :: Parser [ASTExpr]
pparamExprs = do
    eq L.LParen
    eq L.RParen
    return []
    <|>
    do
        eq L.LParen
        p  <- pexpr
        ps <- many $ do
            eq L.Comma
            pexpr
        eq L.RParen
        return (p:ps)

pexpr :: Parser ASTExpr
pexpr = do
    s <- psum
    foldr (\a b -> b <|> a s) empty [return, peq, plt, pgt]
    where
        peq t  = do { eq L.Equ; e <- pexpr; return $ Eq t e }
        plt t  = do { eq L.Lt; e <- pexpr; return $ Lt t e }
        pgt t  = do { eq L.Gt; e <- pexpr; return $ Gt t e }

psum :: Parser ASTExpr
psum = do
    t <- pterm
    foldr (\a b -> b <|> a t) empty [return, padd, psub]
    where
        padd t = do { eq L.Add; e <- psum; return $ Add t e }
        psub t = do { eq L.Sub; e <- psum; return $ Sub t e }

pterm :: Parser ASTExpr
pterm = do
    b <- pbase
    foldr (\a c -> c <|> a b) empty [return, pmul]
    where
        pmul b = do { eq L.Mul; e <- pexpr; return (Mul b e) }

pbase :: Parser ASTExpr
pbase = do
    pcall
    <|>
    pvar
    <|>
    pconst
    where
        pvar   = do { n <- pident; return $ VarExpr n }
        pcall  = do { n <- pident; ps <- pparamExprs; return $ CallExpr n ps }


pconst :: Parser ASTExpr
pconst = do
    pbool
    <|>
    pnumc
    where
        pbool = do { eq L.TTrue; return $ ConstExpr $ Bool True }
            <|> do { eq L.TFalse; return $ ConstExpr $ Bool False }
        pnumc = do
            n <- pnum
            return $ ConstExpr $ Int (read n)



