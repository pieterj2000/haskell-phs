module Main where

import System.Exit
import System.Process
import System.IO (hShow, hGetContents')

-- TODO gebruik detailed test manier: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#example-package-using-detailed-0-9-interface
tests = [
    ("integer1.hs", 1),
    ("integerneg1.hs", -1),
    ("integerneg2.hs", -133)
    ]


doeTest :: Show a => (String, a) -> IO Bool
doeTest (filename, result) = do
    putStr $ "Testing: " ++ filename ++ "... "
    (_, Just hout, _, _) <- createProcess (proc "cabal" 
            ["run", "phs", "--", "tests/instances/"++filename, "interpreterteststand"]
        ){ std_out = CreatePipe }
    output <- init <$> hGetContents' hout -- final newline eraf
    let verwacht = show result
    if output == verwacht
        then putStrLn "Correct" >> return True
        else do
            putStrLn "FAIL"
            putStrLn "verwachtte: "
            putStrLn verwacht
            putStrLn "kreeg: "
            putStrLn output
            return False

main :: IO ()
main = do
    resultaten <- traverse doeTest tests
    if and resultaten
        then putStrLn "alle tests gelukt"
        else putStrLn "niet alle tests gelukt" >> exitFailure