module Main (main) where

import System.Environment (getArgs)

import Parser 

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        (inputfile:_)   -> 
            do  input <- readFile inputfile
                let ast = parseFile input
                    out = show ast
                putStrLn out



