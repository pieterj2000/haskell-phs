{-# LANGUAGE InstanceSigs #-}
module ExprDef
(
    -- Token(..),
    -- Pos(..),
    -- SToken,
    Module(..),
    -- PToken
    Source(..),
    WithSource(..),
    HExpr (..),
    VarInfo(..)
) where
import qualified Data.Map as M


-- type PToken = (SToken, Pos)
-- type SToken = (Token, String)


-- data Token -- TODO Kijken welke allemaal gebruikt worden, de rest eruit
--     = TSemicolon
--     | TBracketOpen
--     | TBracketClose
--     | TNewLine
--     | TWhiteSpace
--     | TVarid
--     | TVarsym
--     | TConid
--     | TModid
--     | TSpecial
--     | TSpecialOp
--     | TCase
--     | TClass
--     | TData
--     | TDefault
--     | TDeriving
--     | TDo
--     | TElse
--     | TIf
--     | TImport
--     | TIn
--     | TInfix
--     | TInfixl
--     | TInfixr
--     | TInstance
--     | TLet
--     | TModule
--     | TNewtype
--     | TOf
--     | TThen
--     | TType
--     | TWhere
--     | TUnderscore
--     | TTEST -- TODO weghalen
--     deriving (Eq, Show) --TODO Show instatnie zelf

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


data Module a = Module {
    moduleName :: String,
    moduleFile :: String,
    moduleImports :: [String],
    moduleExports :: [String],
    moduleDefs :: M.Map String a    
} deriving (Show) -- TODO show instance

data HExpr
    = HInt Integer
    | HVar String
    -- | dit is uitsluitend voor reeksen infix operators waar fixity nog niet voor is bepaald! Anders moet het gewoon HVar gebruiken
    | HInfixOp String 
    -- | dit is uitsluitend voor reeksen infix operators waar fixity nog niet voor is bepaald! Anders moet het gewoon HApply gebruiken
    | HInfixExpr [HExpr]
    | HApply HExpr HExpr 
    deriving (Show)


data VarInfo 
    = Fixity FixityType Int

data FixityType = InfixL | InfixR | InfixN



-- data HCExpr
--     = HCInt Int
--     | HCVar String
--     | HCApply HCExpr HCExpr


{-
okeeee, dit veranderen in een LHS RHS iets
STAP 1: Parsen maar dan alles met een LHS RHS, Dan krijgen we dus een (Module X)
STAP 2: imports pakken, en dan met context van de imports, een Context iets maken, 
        en met behulp van die context X-> HDecl doen (en tevens HDecl fixen dat het écht definitief is)
        a)  Context updaten met type annotations
        b)  Fixity dingen doen
        c)  misschien nog andere dingen doen
        d)  Dan pas de LHR parsen en kijken of er dingen fout gaan
        e)  Bij RHS en let/where kunnen we de Context lokaal updaten en daarmee verder parsen
        f)  Gedurende het proces kunnen we ook direct errors gooien als iets 'onbekend' is in de context
        g)  Dan hebben we uiteindelijk alle definities echt geparsed en in HDecl gepompt
STAP 3(?) Wanneer typechecken? Kan dit al tegelijkertijd met stap 2e/f? Of is dat beter helemaal achteraf?



-}





    
-- data HDecl
--     = HFuncBinding String [Pattern] [Match] HExpr -- name, parameters, TODO guards, definition -- TODO dit moet lazy binding zijn? nog uitzoeken/doen
--     deriving (Show)


-- TODO
data Pattern = Pattern deriving (Show) 
data Match = Match deriving (Show) 


