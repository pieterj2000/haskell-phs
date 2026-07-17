module Main (main) where

import System.Environment (getArgs)

import Parser.Module
import Parser.Lexer 
import LambdaCalc
import Parser.Parser
import Parser.Fixity
import System.Exit (exitFailure)
import Desugar
import Data.Maybe (mapMaybe)
import Defs.ExprDefs (VarStore, VarInfo (..), FixityType (..), Module (..), Decl (..))
import Defs.Haskell (HDecl (..))
import qualified Data.Map as M




main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> return ()
        (inputfile:rest)   -> 
            do  
                let --textpos = withpos input
                    --tokens = mapLeft ($ inputfile) $ tokenize input
                    --ast = parseFile (drop 4 inputfile) input --TODO die drop 4 is nodig om src/ of app/ eruit te halen, moet later goed als je folder aware bent zeg maar
                
                    -- TODO dit mag beter
                    printv wat x = if not (null rest) && head rest == "interpreterteststand" then pure () else putStr (wat ++ ":\t") >> print x
                --print textpos
                moduleeither <- parseFile inputfile printv
                case moduleeither of 
                    (Left e) -> print e >> exitFailure
                    (Right (varinfo, m)) -> do
                        let defs = M.toList $ moduleDefs m
                            core = [ Decl naam exp | (naam, exp) <- defs]
                            lcast = astToLambdaCalc varinfo core
                            lcastbruin = lambdaToDeBruin lcast
                            result = runLambdaCalc lcast
                        printv "lambdacalc ast" lcast
                        printv "debruin ast" lcastbruin
                        printv "resultaat" result
                        putStrLn $ showprettyDeBruin result
                
    -- print test
    -- print $ lambdaToDeBruin test
    -- print $ apply (lambdaToDeBruin test) $ Bprim "a"
    -- print $ evalDeBruin $ apply (lambdaToDeBruin test) $ Bprim "a"
    --print $ apply (apply (lambdaToDeBruin test) $ Bprim "a") $ Bprim "b"


