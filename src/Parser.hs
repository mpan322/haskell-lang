module Parser where
import Control.Applicative
import Data.Char
import qualified Lexer as L
import Debug.Trace

data Parser a = Parser { parse :: [L.Token] -> Maybe ([L.Token], a) }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = do
        v <- p
        return (f v)

instance Applicative Parser where
    -- pure :: a -> Parse a
    pure v = Parser (\s -> Just (s, v))

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) p q = do
        f <- p
        v <- q
        return (f v)

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p f = Parser $ \s -> do
        (s', v) <- parse p s
        let q = f v
        parse q s'

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ \_ -> Nothing

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p q = Parser $ \s -> 
        case parse p s of
            Just r  -> Just r
            Nothing -> parse q s

next :: Parser L.Token
next = Parser helper
    where 
        helper []     = Nothing
        helper (x:xs) = Just (xs, x)

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
    | CallStmt String [ASTExpr] deriving Show

data ASTExpr = VarExpr String
    | ConstExpr String
    | Add ASTExpr ASTExpr
    | Mul ASTExpr ASTExpr
    | Sub ASTExpr ASTExpr
    | CallExpr String [ASTExpr] deriving Show

pprog :: Parser [ASTStmt]
pprog = many pfunDecl

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

passign :: Parser ASTStmt
passign = do
    i <- pident
    eq L.Equ
    e <- pexpr
    return (AssignStmt i e)

plet :: Parser ASTStmt
plet = do
    eq L.Let
    name <- pident
    eq L.Equ
    e <- pexpr
    return (LetStmt name e)

preturn :: Parser ASTStmt
preturn = do
    eq L.Ret
    e <- pexpr
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
    t <- pterm
    do
        eq L.Add
        e <- pexpr
        return (Add t e)
        <|>
        do
            eq L.Sub
            e <- pexpr
            return (Sub t e)
            <|>
            return t

pterm :: Parser ASTExpr
pterm = do
    b <- pbase
    do eq L.Mul
       e <- pexpr
       return (Mul b e)
       <|>
       return b

pbase :: Parser ASTExpr
pbase = do
    name <- pident
    params <- pparamExprs
    return (CallExpr name params)
    <|>
    do
        num <- pnum 
        return (ConstExpr num)
        <|>
        do
            name <- pident
            return (VarExpr name)





