{-# LANGUAGE InstanceSigs #-}
module Error (
    Error(..),
    ParseError(..)
) where

import ExprDef (Pos (..))


data Error
    = ParseError ParseError Pos String -- pos string error 

data ParseError
    = ParseModuleFileName String String -- real module name, expected name
    | ParseUnexpectedEOF String 
    | ParseUnexpected String String -- got, expectged
    | ParseEmpty 
    | ParseUnclosedNComment
    | ParseExpectedEOF
    | LayoutError String

instance Show Error where
    show :: Error -> String
    show (ParseError (ParseModuleFileName mname expected) pos file) = parseError file <> "expected module " <> mname <> " to be named " <> expected
    show (ParseError (ParseUnexpectedEOF expected) pos file)        = parseError' pos file <> "unexpected End-Of-File, expected " <> expected
    show (ParseError (ParseUnexpected got expected) pos file)       = parseError' pos file <> "unexpected input " <> got <> ", expected " <> expected
    show (ParseError ParseEmpty pos file)                           = parseError' pos file <> "empty (alternative instance of) parser. TODO fix better error"
    show (ParseError ParseUnclosedNComment pos file)                = parseError' pos file <> "unclosed {-"
    show (ParseError (LayoutError s) pos file)                      = parseError' pos file <> s

parseError :: String -> String 
parseError file = "Parse error " <> file <> ": "
parseError' :: Pos -> String -> String 
parseError' pos file = "Parse error " <> file <> ":" <> show pos <> ": "