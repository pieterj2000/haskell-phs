module ExprDef
(
    Token(..),
    Pos(..),
    SToken
) where

type SToken = (Token, Pos)

data Token
    = Token
    | BracketOpen
    | Module
    | NewLine
    | WhiteSpace

data Pos = Pos {
    col :: Int,
    row :: Int
}