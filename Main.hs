module Main (main) where

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
                  x      -> do print $ calculate x
                               inputLoop

calculate :: String -> Double
calculate s = 0
