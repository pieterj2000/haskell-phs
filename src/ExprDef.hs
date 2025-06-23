{-# LANGUAGE InstanceSigs #-}
module ExprDef
(
    Token(..),
    -- Pos(..),
    SToken,
    Module(..),
    -- PToken
    Source(..),
    WithSource(..)
) where


-- type PToken = (SToken, Pos)
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

-- data Pos = Pos {
    -- line :: Int,
    -- col :: Int
-- } deriving (Eq)


data Source = Source {
    file :: String,
    line :: Int,
    col :: Int
}

instance Show Source where
    show :: Source -> String 
    show (Source file line col) = file <> ":" <> show line <> ":" <> show col

data WithSource a = WithSource {
    val :: a,
    source :: Source
} deriving (Show) -- TODO show instance zelf doen of mss weghalen


instance Functor WithSource where
  fmap :: (a -> b) -> WithSource a -> WithSource b
  fmap f (WithSource v s) = WithSource (f v) s

-- instance Show Pos where
--   show :: Pos -> String
--   show (Pos line col) = show line <> ":" <> show col


data Module = Module {
    name :: String,
    defs :: [Def],
    deps :: [(Module, [String])] --modules die nodig zijn
} -- deriving (Show) -- TODO show instantie

data Def = Def 
    deriving (Show) --TODO Show instantie
