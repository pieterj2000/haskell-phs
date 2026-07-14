{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Defs.Haskell --TODO exports
where
import Defs.Common (Type)



--import ExprDef


data HDecl
    = HFuncDef String HExpr
    -- TODO hier moet eigenlijk nog context constraints bij (dus zoals data 'Eq a => Set a = ....'), en derivings
    -- maar dat is kijken hoe we types doen, (denk ik bij Decl er in)
    -- TODO dit moeten we herzien zodra we types doen
    | HDataDef DataDef
    | HTypeSig String Type
    deriving (Show)

-- TODO doen
data HPattern
    = HPIntLiteral Integer
    | HPFloatLiteral Double
    | HPCons String [HPattern]
    | HPVar String
    deriving Show


--TODO fancy fase types
data HExpr where
    HInt :: Integer -> HExpr 
    HVar :: String -> HExpr 
    -- | dit is uitsluitend voor reeksen infix operators waar fixity nog niet voor is bepaald! Anders moet het gewoon HVar gebruiken
    HInfixOp :: String -> HExpr 
    -- | dit is uitsluitend voor reeksen infix operators waar fixity nog niet voor is bepaald! Anders moet het gewoon HApply gebruiken
    HInfixExpr :: [HExpr] -> HExpr 
    -- | dit is uitsluitend voor voor de correctheid van reeksen infixexpressions, dit zou niet later voor moeten komen
    HInfixParentheses :: HExpr -> HExpr 
    -- | hier storen we de parameters in van links naar rechts (TODO mischien van rechts naar links beter?), (effectief zelfde als HLambda met hogere arity)
    HMetParams :: [String] -> HExpr -> HExpr
    HApply :: HExpr -> HExpr -> HExpr
    HLambda :: String -> HExpr -> HExpr
    HDataConstructor :: String -> HExpr -- TODO dit zou ook een HVar kunnen zijn?
    HCase :: HExpr -> [(HPattern, HExpr)] -> HExpr
deriving instance Show HExpr


-- TODO hier moet eigenlijk nog context constraints bij (dus zoals data 'Eq a => Set a = ....'), en derivings
-- maar dat is kijken hoe we types doen, (denk ik bij Decl er in?)
-- TODO dit moeten we herzien zodra we types doen
-- TODO deze niet gewoon allemaal in de constructor van HDataDef in HDecl gooien?
data DataDef = DataDef {
        datadefnaam :: String,
        datadeftypevars :: [String],
        datadefconstrs :: [DataConsDef] 
    } deriving (Show)

-- TODO types hierin toevoegen? Afhankelijk van hoe we types gaan doen. We moeten zorgen dat de type parameters hierin ook 
-- echt gedefinieerd zijn in de data decls 
-- TODO labelled fields moeten ook hier nog in, TODO ook strictness flags. 
data DataConsDef = DataConsDef {
    dataconsnaam :: String,
    dataconsarity :: Int --TODO deze mag denk ik weg zodra types geimplementeeerd zijn
} deriving (Show)
