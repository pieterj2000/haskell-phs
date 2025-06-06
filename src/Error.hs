{-# LANGUAGE InstanceSigs #-}
module Error (
    Error(..),
    ParseError(..)
) where

import ExprDef


data Error
    = ParseError ParseError Source 

data ParseError
    = ParseModuleFileName String String -- real module name, expected name
    | ParseUnexpectedEOF String --expected
    | ParseUnexpected String String -- got, expected
    | ParseEmpty 
    | ParseUnclosedNComment
    | ParseExpectedEOF
    | LayoutError String

instance Show Error where
    show :: Error -> String
    show (ParseError (ParseModuleFileName mname expected) src) = parseError src <> "expected module " <> mname <> " to be named " <> expected
    show (ParseError (ParseUnexpectedEOF expected) src)        = parseError src <> "unexpected End-Of-File, expected " <> expected
    show (ParseError (ParseUnexpected got expected) src)       = parseError' src <> "unexpected input " <> got <> ", expected " <> expected
    show (ParseError ParseEmpty src)                           = parseError' src <> "empty (alternative instance of) parser. TODO fix better error"
    show (ParseError ParseUnclosedNComment src)                = parseError' src <> "unclosed {-"
    show (ParseError (LayoutError s) src)                      = parseError' src <> s

parseError :: Source -> String 
parseError src = "Parse error " <> file src <> ": "
parseError' :: Source -> String 
parseError' src = "Parse error " <> show src <> ": "