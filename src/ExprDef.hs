module ExprDef
(
    Token(..),
    Pos(..),
    SToken
) where

type SToken = (Token, Pos)

data Token
    = Token
    | Semicolon
    | BracketOpen
    | BracketClose
    | Module
    | NewLine
    | WhiteSpace
    deriving (Eq)

data Pos = Pos {
    col :: Int,
    row :: Int
} deriving (Eq)