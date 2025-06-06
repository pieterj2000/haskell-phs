module Main (main) where

import System.Environment (getArgs)

import Module 
import Lexer 
import ExprDef (Module(..))
import LambdaCalc
import Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        (inputfile:_)   -> 
            do  input <- readFile inputfile
                let --textpos = withpos input
                    --tokens = mapLeft ($ inputfile) $ tokenize input
                    --ast = parseFile (drop 4 inputfile) input --TODO die drop 4 is nodig om src/ of app/ eruit te halen, moet later goed als je folder aware bent zeg maar
                    ast = parseFile inputfile input
                --print textpos
                print ast
    -- print test
    -- print $ lambdaToDeBruin test
    -- print $ apply (lambdaToDeBruin test) $ Bprim "a"
    -- print $ evalDeBruin $ apply (lambdaToDeBruin test) $ Bprim "a"
    --print $ apply (apply (lambdaToDeBruin test) $ Bprim "a") $ Bprim "b"


