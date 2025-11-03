module Main (main) where

import System.Environment (getArgs)

import Parser.Module
import Parser.Lexer 
import ExprDef (Module(..))
import LambdaCalc
import Parser.Parser


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        (inputfile:rest)   -> 
            do  input <- readFile inputfile
                let --textpos = withpos input
                    --tokens = mapLeft ($ inputfile) $ tokenize input
                    --ast = parseFile (drop 4 inputfile) input --TODO die drop 4 is nodig om src/ of app/ eruit te halen, moet later goed als je folder aware bent zeg maar
                    mast = parseFile inputfile input
                
                    -- TODO dit mag beter
                    printv x = if (length rest > 0 && head rest == "interpreterteststand") then pure () else print x
                --print textpos
                case mast of 
                    (Left e) -> print e
                    (Right ast) -> do
                        let lcast = astToLambdaCalc ast
                            result = runLambdaCalc lcast
                        printv ast
                        printv lcast
                        print result
                
    -- print test
    -- print $ lambdaToDeBruin test
    -- print $ apply (lambdaToDeBruin test) $ Bprim "a"
    -- print $ evalDeBruin $ apply (lambdaToDeBruin test) $ Bprim "a"
    --print $ apply (apply (lambdaToDeBruin test) $ Bprim "a") $ Bprim "b"


