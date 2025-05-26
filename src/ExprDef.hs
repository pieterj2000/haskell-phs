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


data Token
    = TSemicolon
    | TBracketOpen
    | TBracketClose
    | TNewLine
    | TWhiteSpace
    | TVarid
    | TConid
    | TModid
    | TSpecial
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
    col :: Int,
    row :: Int
} deriving (Eq)

instance Show Pos where
  show :: Pos -> String
  show (Pos col row) = show col <> ":" <> show row


data Module = Module {
    name :: String,
    defs :: [Def]
} deriving (Show) -- TODO show instantie

data Def = Def 
    deriving (Show) --TODO Show instantie
