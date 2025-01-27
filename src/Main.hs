module Main where
import Parser
import Lexer
import Interpreter
import System.IO
import Control.Monad

readData :: String -> IO String
readData name = do
    hndl <- openFile name ReadMode
    cnts <- hGetContents hndl
    return cnts

runProg :: String -> IO ()
runProg name = do
    raw <- readData name
    let code = pipeline raw
    case code of
        Left err -> putStrLn $ show err
        Right co -> do
            evaluate co
            return ()
    return ()
    where
        pipeline = lexer >=> parse

main :: IO ()
main = do
    putStrLn "hello\n"
