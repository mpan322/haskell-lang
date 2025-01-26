module Main where
import Parser
import Lexer
import Interpreter

pl :: String -> Either String [ASTStmt]
pl str = 
    case lexer str of
        Left e -> Left (show e)
        Right v -> do
            case parse pprog (snd v) of
                Nothing -> Left "failed to parse"
                Just x  -> Right (snd x)

test :: String -> IO ()
test n = do
    let v = pl n
    case v of
        Left e  -> putStrLn e
        Right v -> do
            putStrLn (show v)
            r <- evaluate v
            putStrLn (show r)

main :: IO ()
main = do
    putStrLn "hello\n"
