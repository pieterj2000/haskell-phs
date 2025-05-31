module Main (main) where

import System.Environment (getArgs)

import Module 
import Lexer (tokenize, withpos)
import ExprDef (Module(..))

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
                let textpos = withpos input
                    tokens = mapLeft ($ inputfile) $ tokenize input
                    ast = parseFile (drop 4 inputfile) input --TODO die drop 4 is nodig om src/ of app/ eruit te halen, moet later goed als je folder aware bent zeg maar
                    out = case ast of
                        Left e  -> show e
                        Right x -> show $ name x
                --print textpos
                print tokens
                putStrLn out



