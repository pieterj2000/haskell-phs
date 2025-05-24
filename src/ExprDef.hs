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
    | TModule
    | TNewLine
    | TWhiteSpace
    deriving (Eq)

data Pos = Pos {
    col :: Int,
    row :: Int
} deriving (Eq)


data Module = Module {
    name :: String,
    defs :: [Def]
}

data Def



{-
dingen in rest gebruikt:
qvarid = [modid . ] varid
qconid = [modid . ] conid
tyvar                       = varid
qtycon = [modid . ] tycon   = conid
qtycls = [modid . ] tycls   = conid
modid                       = conid

lexeme basisding
qvarsym = [modid . ] varsym
qconsym = [modid . ] consym
literal
special
reservedop
reserveid

-}