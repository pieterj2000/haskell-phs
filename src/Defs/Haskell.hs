{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Defs.Haskell --TODO exports
where
import Defs.Common (Type)



--import ExprDef


data HDecl fase maxfase
    = HFuncDef String (HExpr fase maxfase)
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



data Nat = Zero | Succ Nat

type Parsen = Zero
type FixityRegister = Succ Parsen
type FixitySolve = Succ FixityRegister
type ParamsWeg = Succ FixitySolve
type Klaar = Succ ParamsWeg

type family Min (a :: Nat) (b :: Nat) :: Nat where
    Min Zero a = Zero
    Min a Zero = Zero
    Min (Succ a) (Succ b) = Succ (Min a b)

type family Vanaf fase (a :: Nat) where
    Vanaf fase Zero = fase
    Vanaf fase (Succ a) = Succ (Vanaf fase a)

type family Na fase (a :: Nat) where
    Na fase a = Succ (Vanaf fase a)

class (a :: Nat) <= (b :: Nat)
instance Zero <= a
instance a <= b => Succ a <= Succ b

class (a :: Nat) < (b :: Nat)
instance Zero < Succ a
instance a < b => Succ a < Succ b


data FaseList a (faselist :: Nat) (maxfase :: Nat) where
    Nil ::  FaseList a '[] maxfase
    (:<) :: a fase maxfase -> FaseList a faselist lmaxfase -> FaseList a (fase : faselist) (Min maxfase lmaxfase) 
infixr 5 :<
deriving instance Show (FaseList HExpr f maxfase)
deriving instance Show (FaseList (CaseOption HExpr) f maxfase)


newtype CaseOption t (fase :: Nat) (maxfase :: Nat) = CaseOptions (HPattern, t fase maxfase)
    deriving Show


-- | Er moet een extra fase parameter zijn, buiten de (was origineel dé fase parameter) maxfase, omdat je anders niet waardes van fases kan veranderen
data HExpr (fase :: Nat) (maxfase :: Nat) where
    HInt :: Integer -> HExpr fase maxfase
    HVar :: String -> HExpr fase maxfase
    -- | dit is uitsluitend voor reeksen infix operators waar fixity nog niet voor is bepaald! Anders moet het gewoon HVar gebruiken
    HInfixOp :: String -> HExpr fase FixitySolve
    -- | dit is uitsluitend voor reeksen infix operators waar fixity nog niet voor is bepaald! Anders moet het gewoon HApply gebruiken
    HInfixExpr :: FaseList HExpr faselist FixitySolve -> HExpr fase FixitySolve
    -- | dit is uitsluitend voor voor de correctheid van reeksen infixexpressions, dit zou niet later voor moeten komen
    HInfixParentheses :: HExpr fase FixitySolve -> HExpr fase FixitySolve
    -- | hier storen we de parameters in van links naar rechts (TODO mischien van rechts naar links beter?), (effectief zelfde als HLambda met hogere arity)
    HMetParams :: fase1 <= ParamsWeg => [String] -> HExpr fase fase1-> HExpr fase ParamsWeg
    HApply :: HExpr f fase1 -> HExpr f fase2 -> HExpr f (Min fase1 fase2)
    HLambda :: String -> HExpr f maxfase -> HExpr f maxfase
    HDataConstructor :: String -> HExpr f maxfase -- TODO dit zou ook een HVar kunnen zijn?
    HCase :: HExpr f lfase -> FaseList (CaseOption HExpr) faselist rfase -> HExpr f (Min lfase rfase)
deriving instance Show (HExpr f fase)

class NaarFase (a :: Nat -> Nat -> *) where
    naarfase :: forall van naar maxfase. naar <= maxfase => a van maxfase -> a naar maxfase

instance NaarFase HExpr where
    naarfase :: forall van naar maxfase. naar <= maxfase => HExpr van maxfase -> HExpr naar maxfase
    naarfase (HInt i) = HInt i
    naarfase (HVar i) = HVar i
    naarfase (HInfixOp x) = HInfixOp x
    naarfase (HInfixExpr x) = HInfixExpr $ naarfase' x

naarfase' :: forall van naar maxfase. naar <= maxfase => FaseList HExpr van maxfase -> FaseList HExpr naar maxfase
naarfase' Nil = Nil
naarfase' (a :< as) = 
    let a' = naarfase a :: HExpr naar maxfase
        
    in undefined :< undefined -- naarfase a :< naarfase' @van @naar @maxfase as


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
