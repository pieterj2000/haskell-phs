module Main (main) where

import System.Environment (getArgs)

import Parser.Module
import Parser.Lexer 
import ExprDef (Module(..), VarInfo (..), FixityType (..))
import LambdaCalc
import Parser.Parser
import Parser.Fixity
import System.Exit (exitFailure)


varinfoDefault :: [(String, VarInfo)]
varinfoDefault = [
        ("(-)", VarInfo InfixL 6),
        ("(+)", VarInfo InfixL 6)
    ]


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
                    (Left e) -> print e >> exitFailure
                    (Right ast) -> 
                        let varinfo = varinfoDefault -- TODO 
                        in printv ast >> case solveFixity varinfo ast of
                            (Left e) -> print e >> exitFailure
                            (Right fast) -> do
                                let lcast = astToLambdaCalc fast
                                    result = runLambdaCalc lcast
                                printv fast
                                printv lcast
                                printv result
                                putStrLn $ showprettyDeBruin result
                
    -- print test
    -- print $ lambdaToDeBruin test
    -- print $ apply (lambdaToDeBruin test) $ Bprim "a"
    -- print $ evalDeBruin $ apply (lambdaToDeBruin test) $ Bprim "a"
    --print $ apply (apply (lambdaToDeBruin test) $ Bprim "a") $ Bprim "b"


