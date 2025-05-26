{-# LANGUAGE InstanceSigs #-}
module Error (
    Error(..),
    ParseError
) where

import ExprDef (Pos (..))


data Error
    = ParseError ParseError Pos String -- pos string error 

data ParseError
    = ParseModuleFileName String String String -- module name, expected module name, filename
    | ParseUnexpectedEOF String String -- expected token, filename
    | ParseUnexpected String String Pos String -- got token, expected token, position, filename
    | ParseEmpty Pos String -- pos, filename
    | ParseUnclosedNComment Pos String -- pos, filename

instance Show ParseError where
    show :: ParseError -> String
    show (ParseModuleFileName mname expected file)  = parseError file <> "expected module " <> mname <> " to be named " <> expected
    show (ParseUnexpectedEOF expected file)         = parseError file <> "unexpected End-Of-File, expected " <> expected
    show (ParseUnexpected got expected pos file)    = parseError' pos file <> "unexpected input " <> got <> ", expected " <> expected
    show (ParseEmpty pos file)                      = parseError' pos file <> "empty (alternative instance of) parser. TODO fix better error"
    show (ParseUnclosedNComment pos file)           = parseError' pos file <> "unclosed {-"

parseError :: String -> String 
parseError file = "Parse error " <> file <> ": "
parseError' :: Pos -> String -> String 
parseError' (Pos row col) file = "Parse error " <> file <> ":" <> show col <> ":" <> show row <> ": "