{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Error (
    Error(..),
    ParseError(..)
) where

import ExprDef
import Parser.Lexer

-- TODO, moet dit niet geparameteriseerd worden met input type (dus [Token] ipv String)
class ParseError i e | e -> i where
    -- | to be used in empty alternative instance
    emptyError :: e
    -- | expected -> got -> e
    unexpectedError :: String -> String -> e
    -- | unconsumed input -> e
    unconsumedError :: [i] -> e
    fixityError :: e




instance ParseError (Token Char) Error where
  emptyError :: Error
  emptyError = Error "error: empty alternative used"
  unexpectedError :: String -> String -> Error
  unexpectedError expected got = Error ("error, expected " ++ expected ++ ", got " ++ got ++ " instead")
  unconsumedError :: [Token Char] -> Error
  unconsumedError rest = Error $ "error, not consumed all input. Remaining: '" ++ (show rest) ++ "'"


-- addFileLocation :: String -> Error -> Error
-- addFileLocation file (Error s)
-- TODO fatsoenlijk error
data Error = Error String

instance Show Error where
  show :: Error -> String
  show (Error s) = s

-- data Error
--     = ParseError ParseError Source 

-- data ParseError
--     = ParseModuleFileName String String -- real module name, expected name
--     | ParseUnexpectedEOF String --expected
--     | ParseUnexpected String String -- got, expected
--     | ParseEmpty 
--     | ParseUnclosedNComment
--     | ParseExpectedEOF
--     | LayoutError String

-- instance Show Error where
--     show :: Error -> String
--     show (ParseError (ParseModuleFileName mname expected) src) = parseError src <> "expected module " <> mname <> " to be named " <> expected
--     show (ParseError (ParseUnexpectedEOF expected) src)        = parseError src <> "unexpected End-Of-File, expected " <> expected
--     show (ParseError (ParseUnexpected got expected) src)       = parseError' src <> "unexpected input " <> got <> ", expected " <> expected
--     show (ParseError ParseEmpty src)                           = parseError' src <> "empty (alternative instance of) parser. TODO fix better error"
--     show (ParseError ParseUnclosedNComment src)                = parseError' src <> "unclosed {-"
--     show (ParseError (LayoutError s) src)                      = parseError' src <> s

-- parseError :: Source -> String 
-- parseError src = "Parse error " <> file src <> ": "
-- parseError' :: Source -> String 
-- parseError' src = "Parse error " <> show src <> ": "