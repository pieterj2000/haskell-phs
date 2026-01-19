module Desugar 
(
    desugarToCore
)
where

import ExprDef
import Data.List (singleton)
import Control.Arrow (Arrow(..))



paramstolambda :: HDecl -> HDecl
paramstolambda (HFuncDef naam params def) = HFuncDef naam [] $ go (reverse params) def
    where
        go [] expr = expr
        go (p:ps) expr = go ps $ HLambda p expr
paramstolambda x@(HDataDef _) = x
paramstolambda x@(HTypeSig _ _) = x

desugarToCore :: HDecl -> [Decl CExpr]
-- geen parameters meer: 
desugarToCore (HFuncDef naam [] def) = singleton . Decl naam $ exprToCore def
-- nog wel parameters
desugarToCore x@(HFuncDef _ _ _) = desugarToCore $ paramstolambda x
-- TODO wat moeten we hier nog doen met typevars?
-- TODO hoe stoppen we type in core? We stoppen nu alleen nog maar constructor-definities erin
-- TODO ook type toevoegen aan context
desugarToCore (HDataDef d@(DataDef naam typevars cons)) = zipWith (\index n -> Decl n $ CDataCons index d) [0..] $ map dataconsnaam cons
--desugarToCore (HTypeSig naam typ) = 
desugarToCore (HTypeSig _ _) = [] -- TODO doen

exprToCore :: HExpr -> CExpr
-- TODO deze kijken of we deze type-safe kunenn maken, ipv error
exprToCore (HInfixExpr _) = error "deze zou niet meer voor moeten komen"
exprToCore (HInfixParentheses _) = error "deze zou niet meer voor moeten komen"
exprToCore (HInfixOp _) = error "deze zou niet meer voor moeten komen"
exprToCore (HInt i) = CInt i
exprToCore (HVar naam) = CVar naam
exprToCore (HApply a b) = CApply (exprToCore a) (exprToCore b)
exprToCore (HLambda n e) = CLambda n (exprToCore e)
exprToCore (HDataConstructor naam) = CVar naam -- TODO, voor HDataConstructor niet gewoon HVar gebruiken?
exprToCore (HCase e opties) = CCase (exprToCore e) $ map (second exprToCore) opties


-- [Decl ([String], CExpr)]


-- In het geval van een Decl moeten we één voor één de parameters naar Lambda expressies vervangen.
-- Als het geen parameters meer heeft kunnen we de defintie in context opslaan
-- TODO dit is tevens meest simplisitisch, moet slimmer kunnen
--astToLambdaCalc ctx (Decl naam ([], def) : rest) = astToLambdaCalc (varStoreSetDef naam def ctx) rest
--astToLambdaCalc ctx (Decl naam ((p:ps), def) : rest) = astToLambdaCalc ctx (Decl naam (ps, HLambda p def) : rest)