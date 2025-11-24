module Main (main) where

import System.Environment (getArgs)

import Parser.Module
import Parser.Lexer 
import ExprDef (Module(..), VarInfo (..), FixityType (..), VarStore)
import LambdaCalc
import Parser.Parser
import Parser.Fixity
import System.Exit (exitFailure)
import Desugar


varinfoDefault :: VarStore
varinfoDefault = let (-->) = (,) in
    [ "(-)" --> VarInfo InfixL 6 Nothing
    , "(+)" --> VarInfo InfixL 6 Nothing
    , "(*)" --> VarInfo InfixL 7 Nothing
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
                    printv wat x = if (length rest > 0 && head rest == "interpreterteststand") then pure () else putStr (wat ++ ":\t") >> print x
                --print textpos
                printv "input" $ input
                printv "tokens" $ tokenize input
                case mast of 
                    (Left e) -> print e >> exitFailure
                    (Right ast) -> 
                        let varinfo = varinfoDefault -- TODO 
                        in printv "ast" ast >> case solveFixity varinfo ast of
                            (Left e) -> print e >> exitFailure
                            (Right fast) -> do
                                let core = map desugarToCore fast
                                    lcast = astToLambdaCalc varinfo core
                                    lcastbruin = lambdaToDeBruin lcast
                                    result = runLambdaCalc lcast
                                printv "ast na fixity" fast
                                printv "lambdacalc ast" lcast
                                printv "debruin ast" lcastbruin
                                printv "resultaat" result
                                putStrLn $ showprettyDeBruin result
                
    -- print test
    -- print $ lambdaToDeBruin test
    -- print $ apply (lambdaToDeBruin test) $ Bprim "a"
    -- print $ evalDeBruin $ apply (lambdaToDeBruin test) $ Bprim "a"
    --print $ apply (apply (lambdaToDeBruin test) $ Bprim "a") $ Bprim "b"


