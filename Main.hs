module Main (main) where

import Lexer
import Parser
import Interpreter

main = do
    putStrLn "CALCULATOR"
    putStr   "Type expressions to get their result; "
    putStrLn "type `exit' to exit."
    inputLoop

inputLoop :: IO ()
inputLoop = do
    putStrLn ""
    input <- getLine
    case input of "exit" -> return ()
                  ""     -> inputLoop
                  x      -> do printResult x
                               inputLoop

calculate :: String -> Either String Double
calculate x = tokenize x >>= parse >>= evalExpr

printResult :: String -> IO ()
printResult x = case calculate x of
                     Left err -> putStrLn $ "Error: " ++ err
                     Right result -> print result
