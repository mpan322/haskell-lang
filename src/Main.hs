module Main where
import Parser
import Lexer
import Interpreter
import System.IO

pl :: String -> Either String [ASTStmt]
pl str =
    case lexer str of
        Left e -> Left (show e)
        Right v -> do
            case parse pprog (snd v) of
                Left e -> Left (show e)
                Right x -> Right (snd x)

test :: String -> IO ()
test n = do
    let v = pl n
    case v of
        Left e  -> putStrLn e
        Right v -> do
            putStrLn (show v)
            r <- evaluate v
            putStrLn (show r)

-- runProg :: String -> Either String [ASTStmt]
-- runProg = do
--     toks <- lexer conts
--     parse

-- execProg :: String -> IO ()
-- execProg file = do
--     handle <- openFile file ReadMode
--     conts <- hGetContents handle
--     let lexed = lexer conts >>= parse pprog
--     putStrLn $ show lexed

main :: IO ()
main = do
    putStrLn "hello\n"
