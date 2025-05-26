module Main (main) where

import System.Environment (getArgs)

import Parser 
import Lexer (tokenize)

-- TODO verplaatsen naar iets van utils?
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        (inputfile:_)   -> 
            do  input <- readFile inputfile
                let tokens = mapLeft ($ inputfile) $ tokenize input
                    ast = parseFile inputfile input
                    out = case ast of
                        Left e  -> show e
                        Right x -> show x
                print tokens
                putStrLn out



