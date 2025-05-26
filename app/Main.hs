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
                let ast = parseFile inputfile input
                    out = case ast of
                        Left e  -> show e
                        Right x -> show x
                putStrLn out



