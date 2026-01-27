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
import Defs.ExprDefs (VarStore, VarInfo (..), FixityType (..))
import Defs.Haskell (HDecl (..))


varinfoDefault :: VarStore
varinfoDefault = let (-->) = (,) in
    [ "(-)" --> VarInfo InfixL 6 Nothing
    , "(+)" --> VarInfo InfixL 6 Nothing
    , "(*)" --> VarInfo InfixL 7 Nothing
    ]

-- TODO ergens een check dat er maar één definitie, type, en infix declaration per functie is....

--TODO naar andere file (misschien iets van een Parser.Postprocess, of misscien naar Parser, en dan alles wat in de huidige Parser staat naar Parser.Parser of zo?)
-- | adds all fixity declaration to the variable store, so that sequences of infix operations 
--   can be resolved. Note that this does not add definitions yet to the varstore
--TODO ook daadwerkelijke infix opslaan zodra infix declarations ook echt werken :)
--TODO alle types van HDecl doen
registerFixity :: VarStore -> [HDecl] -> VarStore
registerFixity vars decls = 
    let fundefs = mapMaybe (\x -> case x of { (HFuncDef naam _ _) -> Just naam; _ -> Nothing }) decls
    in foldr (\naam store -> (naam, VarInfo InfixL 9 Nothing) : store) vars fundefs




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
                        in do
                            printv "ast" ast
                            let varinfof = registerFixity varinfo ast
                            case solveFixity varinfof ast of
                                (Left e) -> print e >> exitFailure
                                (Right fast) -> do
                                    let core = concatMap desugarToCore fast
                                        lcast = astToLambdaCalc varinfo core
                                        lcastbruin = lambdaToDeBruin lcast
                                        result = runLambdaCalc lcast
                                    printv "ast na fixity" fast
                                    printv "core" core
                                    --printv "lambdacalc ast" lcast
                                    --printv "debruin ast" lcastbruin
                                    printv "resultaat" result
                                    putStrLn $ showprettyDeBruin result
                
    -- print test
    -- print $ lambdaToDeBruin test
    -- print $ apply (lambdaToDeBruin test) $ Bprim "a"
    -- print $ evalDeBruin $ apply (lambdaToDeBruin test) $ Bprim "a"
    --print $ apply (apply (lambdaToDeBruin test) $ Bprim "a") $ Bprim "b"


