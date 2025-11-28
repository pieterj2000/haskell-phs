module Main where

import System.Exit
import System.Process
import System.IO (hShow, hGetContents')
import System.Directory
import Data.Maybe (isNothing, fromJust, isJust)
import Prelude hiding (fail)

-- TODO gebruik detailed test manier: https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#example-package-using-detailed-0-9-interface
tests =
    [ ("integer1.hs", Just 1)
    , ("integerneg1.hs", Just $ -1)
    , ("integerneg2.hs", Just $ -1333)
    , ("minusfixity1.hs", Just 1)
    , ("minusfixity2.hs", Nothing)
    , ("minusfixity3.hs", Just $ -1)
    , ("fixity1.hs", Just $ -4)
    , ("simpelfuncs1.hs", Just 478)
    , ("simpelfuncs2.hs", Just 478)
    , ("simpelfuncs3.hs", Just $ -99)
    , ("infixfunc.hs", Just 478)
    , ("facttest.hs", Just 3628800)
    ]

doeTest :: Show a => (String, Maybe a) -> IO Bool
doeTest (filename, result) = do
    putStr $ "Testing: " ++ filename ++ "... "
    (_, Just hout, _, process_handle) <- createProcess (proc "cabal" 
            ["run", "phs", "--", "tests/instances/"++filename, "interpreterteststand"]
        ){ std_out = CreatePipe }
    output <- init <$> hGetContents' hout -- final newline eraf
    exitcode <- waitForProcess process_handle 
    let verwacht = show $ fromJust result
        verwachtcrash = isNothing result
        r   | (verwachtcrash && exitcode /= ExitSuccess) = success
            | (verwachtcrash && exitcode == ExitSuccess) = fail "error" output
            | (output /= verwacht) = fail verwacht output
            | otherwise = success
    r

success :: IO Bool
success = putStrLn "Correct" >> return True

fail :: String -> String -> IO Bool
fail verwacht kreeg = do
    putStrLn "FAIL"
    putStrLn "verwachtte: "
    putStrLn verwacht
    putStrLn "kreeg: "
    putStrLn kreeg
    return False


checkAllTests :: IO ()
checkAllTests = do
    files <- listDirectory "tests/instances"
    print files
    let missingtests = filter (isNothing . flip lookup tests) files
    if null missingtests
        then return ()
        else do
            putStrLn "WARNING: Er zijn testbestanden waar geen tests voor zijn..., het gaat om:" 
            traverse print missingtests
            exitFailure




main :: IO ()
main = do
    checkAllTests
    resultaten <- traverse doeTest tests
    if and resultaten
        then putStrLn "alle tests gelukt"
        else putStrLn "niet alle tests gelukt" >> exitFailure