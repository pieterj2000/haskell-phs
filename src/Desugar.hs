module Desugar 
(
    desugarToCore
)
where

import ExprDef
import Data.List (singleton)

scottEncoding :: [DataConsDef] -> [Decl CExpr]
scottEncoding cons = 
    let aantalcons = length cons
    in zipWith (\c i -> scottEncodeCons aantalcons i c) cons [1..]

-- | amount of constructors -> index of this constructor -> ConsDef -> CExpr
scottEncodeCons :: Int -> Int -> DataConsDef -> Decl CExpr
scottEncodeCons n i (DataConsDef naam arity) =
    let paramnames = map (\k -> "x" ++ show k) [1..arity]
        consnames = map (\k -> "c" ++ show k) [1..n]
        lambdanames = paramnames ++ consnames
        deze = "c" ++ show i
        body = foldl' (\acc el -> CApply acc (CVar el)) (CVar deze) paramnames
        expr = foldr (\el acc -> CLambda el acc) body lambdanames
    in Decl naam expr


paramstolambda :: HDecl -> HDecl
paramstolambda (HFuncDef naam params def) = HFuncDef naam [] $ go (reverse params) def
    where
        go [] expr = expr
        go (p:ps) expr = go ps $ HLambda p expr
paramstolambda x@(HDataDef _) = x

desugarToCore :: HDecl -> [Decl CExpr]
-- geen parameters meer: 
desugarToCore (HFuncDef naam [] def) = singleton . Decl naam $ exprToCore def
-- nog wel parameters
desugarToCore x@(HFuncDef _ _ _) = desugarToCore $ paramstolambda x
-- omzetten naar scott encoding 
-- TODO core moet eigenlijk gewoon data constructors hebben, en naar scott encoding moet pas in de lambdacalc conversion of een latere IR pas komen
-- TODO wat moeten we hier nog doen met typevars?
-- TODO hoe stoppen we type in core? We stoppen nu alleen nog maar constructor-definities erin
desugarToCore (HDataDef (DataDef naam typevars cons)) = scottEncoding cons

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


-- [Decl ([String], CExpr)]


-- In het geval van een Decl moeten we één voor één de parameters naar Lambda expressies vervangen.
-- Als het geen parameters meer heeft kunnen we de defintie in context opslaan
-- TODO dit is tevens meest simplisitisch, moet slimmer kunnen
--astToLambdaCalc ctx (Decl naam ([], def) : rest) = astToLambdaCalc (varStoreSetDef naam def ctx) rest
--astToLambdaCalc ctx (Decl naam ((p:ps), def) : rest) = astToLambdaCalc ctx (Decl naam (ps, HLambda p def) : rest)