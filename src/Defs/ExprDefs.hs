{-# LANGUAGE InstanceSigs #-}
module Defs.ExprDefs
(
    -- Token(..),
    -- Pos(..),
    -- SToken,
    Module(..),
    -- PToken
    Source(..),
    WithSource(..),
    VarInfo(..),
    FixityType(..),
    VarStore,
    varStoreSetDef,
    Decl(..),
    CExpr (..),
    DataDef(..),
    HPattern (..),
) where
import qualified Data.Map as M
import Defs.Haskell (DataDef, HPattern)




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

data Decl a
    = Decl String a 
    deriving (Show)

instance Functor Decl where
  fmap :: (a -> b) -> Decl a -> Decl b
  fmap f (Decl naam x) = Decl naam (f x)


-- TODO willen we hier Data constructors en pattern matching in hebben? Of meteen al een encoding (bijv scott) aan geven
-- TODO willen we specializaties van bijvoorbeeld lists of zo hierin houden?
-- TODO willen we hier let(recs) in stoppen?
-- TODO fixpoint combinator (indien we niet letrecs accepteren)
data CExpr
    = CInt Integer
    | CVar String
    | CApply CExpr CExpr
    | CLambda String CExpr
    | CDataCons Int DataDef  --TODO hier de goeie data structure geven. Weet niet wat precies nodig is in de toekomst... Misschien alleen type dat dat voldoende is  -- TODO: Is deze nodig?
    | CCase CExpr [(HPattern, CExpr)] -- TODO de string moet een pattern worden
    | CLet [DataDef] CExpr -- TODO de DataDef moet misschien iets anders zijn?
    | CLetRec [DataDef] CExpr -- TODO hoe willen we dit aanpakken?
    deriving (Show)

data CLExpr
    = CLInt Integer
    | CLVar String
    | CLApply CExpr CExpr
    | CLLambda String CExpr
    deriving (Show)


-- traverseHExpr :: Applicative f => (HExpr -> f HExpr) -> HExpr -> f HExpr
-- traverseHExpr f ()

-- TODO betere opslag (lees: Map)
type VarStore = [(String, VarInfo)]

-- | verwijdert huidige definitie als aanwezig, en slaat dan nieuwe op
varStoreAddNew :: String -> VarInfo -> VarStore -> VarStore
varStoreAddNew var info store = (var, info) : filter ((var==) . fst) store

-- | Als huidig bestaat, vervangt de defintie, anders voegt standaard toe en zet definitie bij die
varStoreSetDef :: String -> CExpr -> VarStore -> VarStore
varStoreSetDef naam expr = varStoreModifyDefault naam (\info -> info { varDefinition = Just expr })

-- -- | modifies current if exist, anders voeg gegeven toe.
-- --   naam -> modifyFun -> newIfNotExist -> huidigVarStore -> aangepasteVarStore
-- varStoreModifyAdd :: String -> (VarInfo -> VarInfo) -> VarInfo -> VarStore -> VarStore
-- varStoreModifyAdd naam modFun new store = case lookup naam store of
--     Nothing -> (naam, new) : store
--     Just info -> varStoreAddNew naam (modFun info) store

-- -- | voeg default toe voor gegeven naam
-- varStoreAddDefault :: String -> VarStore -> VarStore
-- varStoreAddDefault naam store = (naam, VarInfo InfixL 9 Nothing) : store

-- | modifies current if exists, anders insert default en modify die
varStoreModifyDefault :: String -> (VarInfo -> VarInfo) -> VarStore -> VarStore
varStoreModifyDefault naam f store = case lookup naam store of
    Nothing -> (naam, f $ VarInfo InfixL 9 Nothing) : store
    Just info -> varStoreAddNew naam (f info) store


--TODO misschien moet hier geparameteriseerd worden over wat voor type expression het is, nu is het een CExpr, maar misschien ooit iets anders
data VarInfo = VarInfo {
    varFixity :: FixityType,
    varFixityPrecedence :: Int,
    -- | Nothing betekent dat het een primitieve is...
    varDefinition :: Maybe CExpr
}

data FixityType = InfixL | InfixR | InfixN deriving (Show, Eq)



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


