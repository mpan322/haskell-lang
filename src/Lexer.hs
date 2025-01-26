module Lexer where
import Control.Applicative
import Data.Char

-- metadata about token location in source code for errors
type TokenData = (Int, Int)

data LexError = 
    UnknownError
    | UnknownToken
    deriving (Show)

data Token = Fun
    | Add
    | Sub
    | Mul
    | Let
    | Equ
    | Ret
    | Ident String
    | Num String
    | LParen
    | RParen
    | LBrack
    | RBrack
    | Comma
    | Semi
    deriving (Show, Eq)

newtype Lexer a = Lexer { runLexer :: String -> Either LexError (String, a) }

instance Functor Lexer where
    fmap f l = do
        v <- l
        return (f v)

instance Applicative Lexer where
    pure v = Lexer $ \s -> Right (s, v)

    (<*>) m l = do
        f <- m
        v <- l
        return (f v)

instance Monad Lexer where
    return = pure
    (>>=) p f = Lexer $ \s -> do
        (s', v) <- runLexer p s 
        let q = f v
        runLexer q s'

instance Alternative Lexer where
    empty = Lexer $ \s -> Left UnknownError
    (<|>) l r = Lexer $ \s -> 
        case runLexer l s of
            g@(Right _) -> g
            _           -> runLexer r s

next :: Lexer Char
next = Lexer helper
    where
        helper []     = Left UnknownError
        helper (x:xs) = Right (xs, x)

peek :: Lexer Char
peek = Lexer helper
    where
        helper []       = Left UnknownError
        helper s@(x:xs) = Right (s, x)

isEnd :: Lexer Bool
isEnd = Lexer helper
    where
        helper [] = Right ([], True)
        helper s  = Right (s, False)

sat :: (Char -> Bool) -> Lexer Char
sat p = do
    v <- next
    if p v then return v
    else empty

token :: Token -> String -> Lexer Token
token tok str = do
    helper str
    return tok
    where
        helper :: String -> Lexer ()
        helper [] = return ()
        helper (x:xs) = do
            sat (==x)
            helper xs

tokenIdent :: Lexer Token
tokenIdent = do
    i <- some (sat isAlpha)
    return (Ident i)

tokenNum :: Lexer Token
tokenNum = do
    i <- some (sat isNumber)
    return (Num i)

eatSpaces :: Lexer ()
eatSpaces = do { many (sat isSpace); return () }

tokenSpace :: Token -> String -> Lexer Token
tokenSpace tok str = do
    res <- token tok str
    con <- helper
    if con then return res
    else empty
    where
        helper :: Lexer Bool
        helper = do
            p <- peek
            return (isSpace p)
            <|>
            isEnd

lexToken :: Lexer Token
lexToken = do
    tokenSpace Let "let"
    <|> tokenSpace Fun "fun"
    <|> tokenSpace Ret "return"
    <|> token Add "+"
    <|> token Sub "-"
    <|> token Mul "*"
    <|> token Equ "="
    <|> token LParen "("
    <|> token RParen ")"
    <|> token LBrack "{"
    <|> token RBrack "}"
    <|> token Comma ","
    <|> token Semi ";"
    <|> tokenIdent
    <|> tokenNum

lexer :: String -> Either LexError (String, [Token])
lexer str = runLexer helper str
    where
        helper = many $ do
            eatSpaces
            o <- lexToken
            eatSpaces
            return o


