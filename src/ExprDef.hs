{-# LANGUAGE InstanceSigs #-}
module ExprDef
(
    Token(..),
    Pos(..),
    SToken,
    Module(..),
    PToken
) where


type PToken = (SToken, Pos)
type SToken = (Token, String)


data Token -- TODO Kijken welke allemaal gebruikt worden, de rest eruit
    = TSemicolon
    | TBracketOpen
    | TBracketClose
    | TNewLine
    | TWhiteSpace
    | TVarid
    | TVarsym
    | TConid
    | TModid
    | TSpecial
    | TSpecialOp
    | TCase
    | TClass
    | TData
    | TDefault
    | TDeriving
    | TDo
    | TElse
    | TIf
    | TImport
    | TIn
    | TInfix
    | TInfixl
    | TInfixr
    | TInstance
    | TLet
    | TModule
    | TNewtype
    | TOf
    | TThen
    | TType
    | TWhere
    | TUnderscore
    | TTEST -- TODO weghalen
    deriving (Eq, Show) --TODO Show instatnie zelf

data Pos = Pos {
    line :: Int,
    col :: Int
} deriving (Eq)

instance Show Pos where
  show :: Pos -> String
  show (Pos line col) = show line <> ":" <> show col


data Module = Module {
    name :: String,
    defs :: [Def],
    deps :: [(Module, [String])] --modules die nodig zijn
} -- deriving (Show) -- TODO show instantie

data Def = Def 
    deriving (Show) --TODO Show instantie
