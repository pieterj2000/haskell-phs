module ExprDef
(
    Token(..),
    Pos(..),
    SToken,
    Module(..)
) where


type SToken = (Token, Pos)

data Token
    = TSemicolon
    | TBracketOpen
    | TBracketClose
    | TNewLine
    | TWhiteSpace
    | TVarid { getVarid :: String }
    | TConid { getConid :: String }
    | TModid { getModid :: String }
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
    deriving (Eq, Show) --TODO Show instatnie zelf

data Pos = Pos {
    col :: Int,
    row :: Int
} deriving (Eq)


data Module = Module {
    name :: String,
    defs :: [Def]
} deriving (Show) -- TODO show instantie

data Def = Def 
    deriving (Show) --TODO Show instantie
