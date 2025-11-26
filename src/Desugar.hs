module Desugar 
(
    desugarToCore
)
where

import ExprDef


desugarToCore :: HDecl -> Decl CExpr
-- geen parameters meer: 
desugarToCore (HFuncDef naam [] def) = Decl naam $ exprToCore def
desugarToCore (HFuncDef naam (p:ps) def) = desugarToCore $ HFuncDef naam ps $ HLambda p def


exprToCore :: HExpr -> CExpr
-- TODO deze kijken of we deze type-safe kunenn maken, ipv error
exprToCore (HInfixExpr _) = error "deze zou niet meer voor moeten komen"
exprToCore (HInfixParentheses _) = error "deze zou niet meer voor moeten komen"
exprToCore (HInfixOp _) = error "deze zou niet meer voor moeten komen"
exprToCore (HInt i) = CInt i
exprToCore (HVar naam) = CVar naam
exprToCore (HApply a b) = CApply (exprToCore a) (exprToCore b)
exprToCore (HLambda n e) = CLambda n (exprToCore e)


-- [Decl ([String], CExpr)]


-- In het geval van een Decl moeten we één voor één de parameters naar Lambda expressies vervangen.
-- Als het geen parameters meer heeft kunnen we de defintie in context opslaan
-- TODO dit is tevens meest simplisitisch, moet slimmer kunnen
--astToLambdaCalc ctx (Decl naam ([], def) : rest) = astToLambdaCalc (varStoreSetDef naam def ctx) rest
--astToLambdaCalc ctx (Decl naam ((p:ps), def) : rest) = astToLambdaCalc ctx (Decl naam (ps, HLambda p def) : rest)