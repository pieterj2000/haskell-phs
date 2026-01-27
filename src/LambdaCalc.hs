module LambdaCalc where -- TODO exportlijst

import Defs.ExprDefs
import qualified Debug.Trace as Debug
import Control.Arrow (Arrow(..))
import Defs.Haskell (HPattern(..), DataConsDef (..), DataDef (datadefconstrs))





-- TODO dit bouwt één hele grote lambda expressie voor het hele programma (specifiek voor main)
-- is het niet beter om gewoon een [Decl LambdaCalc] te maken? en dan bij het interpretern van die lambdacalculus
-- (of welke intepreter/runtime we uiteindlijk gaan doen) pas alle definities te substitueren? Over nadenken
-- Of misschien is dit wel prima, maar dan is deze 'astToLambdaCalc' eigenlijk specifiek een 
--  'toLambdaCalcZonderSharingZonderFixPointAlsÉénGroteExpressie'
-- TODO wss zullen niet alle runtimes/interpreters alle primitieven begrijpen, dus wellicht moeten we daar wat mee
-- bijvorbeeld alsnog definities van primitieven-shims meegeven?
--TODO eerst naar core!
 -- TODO recursie eruithalen pass --TODO misschien beter die ergens anders neer te zetten? 
-- Oke recursie zou denk ik alsnog moeten werken denk ik, want hij genereerd in principe dan een oneindige ast
-- door oneindig de definitie te vervangen, maar zolang alle functies t/m evalueren lazy zijn, zal hij pas 
-- genereren tot nodig, en als de recursie eindig is, zal hij ook terminaten.
-- Ik denk dat dat ook zou moeten werken bij letrecs.
-- Maarrrrr, dan gebruikt het dus geen sharing
-- En maarrrrr, als we ooit het wel meer willen compilen, dan moeten we het uiteindelijk naar een primitieve fixpoint operator brengen
-- en die moeten we dan ook gebruiken bij recursieve definities/letrecs
astToLambdaCalc :: VarStore -> [Decl CExpr] -> LambdaCalc
-- Er zijn geen expressions meer te converten, dus main evalueren
astToLambdaCalc ctx [] = astToLambdaCalc' ctx (CVar "main")
astToLambdaCalc ctx (Decl naam def : rest) = astToLambdaCalc (varStoreSetDef naam def ctx) rest


scottEncoding :: [DataConsDef] -> [CExpr] --[Decl CExpr]
scottEncoding cons = 
    let aantalcons = length cons
    in zipWith (\c i -> scottEncodeCons aantalcons i c) cons [1..]

-- | amount of constructors -> index of this constructor -> ConsDef -> CExpr
scottEncodeCons :: Int -> Int -> DataConsDef -> CExpr --Decl CExpr
scottEncodeCons n i (DataConsDef naam arity) =
    let paramnames = map (\k -> "x" ++ show k) [1..arity]
        consnames = map (\k -> "c" ++ show k) [1..n]
        lambdanames = paramnames ++ consnames
        deze = "c" ++ show i
        body = foldl' (\acc el -> CApply acc (CVar el)) (CVar deze) paramnames
        expr = foldr (\el acc -> CLambda el acc) body lambdanames
    in expr -- Decl naam expr


-- TODO: Precondition is dat het één enkele case distinction is, en de rest allemaal met desugaren is weggewerkt. Eigenlijk moet HPattern een andere type zijn die dat impliciet heeft
patternMatchToLambda :: VarStore -> CExpr -> [(HPattern, CExpr)] -> LambdaCalc
patternMatchToLambda ctx exp spul =
    let expl = astToLambdaCalc' ctx exp
        spull = map (second $ astToLambdaCalc' ctx) spul
        go lexp [] = lexp
        go lexp ((HPCons consnaam vars, doelexp) : rest) = go (Lapply lexp rexp) rest
            where
                rexp = foldr (\(HPVar v) e -> Llambda v e) doelexp vars
        go lexp (_ : rest) = error "zou niet meer voor moeten komen"

    in go expl spull



astToLambdaCalc' :: VarStore -> CExpr -> LambdaCalc
astToLambdaCalc' ctx (CInt x) = Lint x 
astToLambdaCalc' ctx (CLambda naam def) = Llambda naam $ astToLambdaCalc' ctx def
astToLambdaCalc' ctx (CApply f x) = Lapply (astToLambdaCalc' ctx f) (astToLambdaCalc' ctx x)
-- Kijk of het een bekende variable is, en substitueer de definitie (TODO dit is de meest simplistische versie)
--TODO recursieve definities zullen nu niet goed gaan. 
-- Er moet dus vooraf een 'remove recursion' pass over de AST komen, vóór de vertaling
-- Oke recursie zal nu wel werken, zie comment bij astToLambdaCalc, maar dit is zeker niet optimaal
astToLambdaCalc' ctx (CVar x) = case lookup x ctx >>= varDefinition of 
    Nothing -> Lvar x 
    Just def -> astToLambdaCalc' ctx def
astToLambdaCalc' ctx (CDataCons index datadef) = 
    let numcons = length $ datadefconstrs datadef 
        con = datadefconstrs datadef !! index -- TODO aanpassen dit is stom
    in astToLambdaCalc' ctx  $ scottEncodeCons numcons index con
astToLambdaCalc' ctx (CCase e opties) = patternMatchToLambda ctx e opties


-- TODO DOEN
astToLambdaCalc' ctx (CLet _ _) = undefined
astToLambdaCalc' ctx (CLetRec _ _) = undefined




runLambdaCalc :: LambdaCalc -> DeBruin
runLambdaCalc = evalDeBruin . lambdaToDeBruin


data LambdaCalc
    = Lvar String
    | Llambda String LambdaCalc
    | Lapply LambdaCalc LambdaCalc
    | Lint Integer

data DeBruin
    = Bprim String
    | Bvar Int
    | Blambda DeBruin
    | Bapply DeBruin DeBruin
    | Bint Integer
    deriving(Show)
    
instance Show LambdaCalc where
  showsPrec p (Lvar s) = showString s
  showsPrec p (Llambda s l) = showParen (p > 0) $ showChar '\\' . showString s . showChar '.' . shows l
  showsPrec p (Lapply a b) = showParen (p > 0) $ showsPrec 1 a . showChar ' ' . showsPrec 1 b
  showsPrec p (Lint x) = shows x


-- instance Show DeBruin where
showprettyDeBruin :: DeBruin -> String
showprettyDeBruin = ($ "") . showprettyDeBruin' 0
showprettyDeBruin' :: Int -> DeBruin -> (String -> String)
showprettyDeBruin' p (Bprim s) = (if p > 1 then (' ':) else id) . showString s
showprettyDeBruin' p (Bvar i) = showParen (i>9) (shows i)
showprettyDeBruin' p (Blambda l) = showParen (p > 0) (showChar '\\' . shows l)
showprettyDeBruin' p (Bapply a b) = showParen (p > 0) (showprettyDeBruin' 1 a . showprettyDeBruin' 2 b)
showprettyDeBruin' p (Bint x) = shows x

test = Llambda "f" $ Lapply (Llambda "x" $ Lapply (Lvar "x") (Lvar "x")) (Llambda "x" $ Lapply (Lvar "f") $ Lapply (Lvar "x") (Lvar "y"))

lambdaToDeBruin :: LambdaCalc -> DeBruin
lambdaToDeBruin = go []
    where
        go context (Lvar x) = case lookup x $ zip context [0..] of
                                    Just i -> Bvar i
                                    Nothing -> Bprim x
        go context (Llambda x y) = Blambda $ go (x:context) y
        go context (Lapply x y) = Bapply (go context x) (go context y)
        go context (Lint x) = Bint x

-- applies b to a lambda term a. Recurses down tree and changes each instance of a with b
applyToLambda :: DeBruin -> DeBruin -> DeBruin
applyToLambda (Blambda a) b = go a 0
    where
        go (Bprim s) _      = Bprim s
        go (Bint x) _       = Bint x
        go (Bvar i) j       = if i == j then b else Bvar i
        go (Blambda l) j    = Blambda $ go l (j+1)
        go (Bapply f x) j   = Bapply (go f j) (go x j)
applyToLambda f x = error $ "applying to something different than a lambda. Applying: '" ++ show f ++ "' with '" ++ show x ++ "'"

evalDeBruin :: DeBruin -> DeBruin
evalDeBruin = fromSpine . evalDeBruin' . makeSpine

-- is in de vorm van [linksonder aan top spine, rechtsonder aan top spine, rechtsonder aan (top-1) spine, ...]
makeSpine :: DeBruin -> [DeBruin]
makeSpine = reverse . go
    where
        go (Bapply f x) = x : (go f)
        go anders = [anders] -- We stoppen de linker node van de laatste application op het begin

fromSpine :: [DeBruin] -> DeBruin
fromSpine [] = error "zou niet moeten gebeuren"
fromSpine (eerste:daarna) = go eerste daarna
    where 
        go links [] = links
        go links (rechts:rest) = go (Bapply links rechts) rest


-- evals de bruin naively, i.e. no sharing
evalDeBruin' :: [DeBruin] -> [DeBruin]
evalDeBruin' [] = error "empty ding zou niet moeten kunnen"
-- Het zou een apply kunnen zijn na de application van een lambda abstraction
evalDeBruin' [Bapply f x] = evalDeBruin' $ makeSpine (Bapply f x)
-- anders kunnen we het niet oplossen :)
evalDeBruin' [x] = [x] 
-- als de top van de spine links een lambda is, dan kunnen we gewoon die application uitvoeren
evalDeBruin' (Blambda l : x : rest) = evalDeBruin' $ (applyToLambda (Blambda l) x) : rest

evalDeBruin' alles@(Bprim p : rest) = case lookup p primitives of
        Nothing -> error $ "undefined primitive '" ++ p ++ "'" -- TODO fatsoenlijke error maken, of iets mee doen
        Just (arity, fun) ->
            let (params, overig) = splitAt arity rest
                paramsNormalized = map evalDeBruin params
                isInt (Bint _) = True -- TODO deze check moet veranderen als we ook primitieve voor andere types hebben. 
                                        -- misschien in de primitives lijst een eigen checkfunctie voor iedere primitive?
                isInt _ = False
                fromInt (Bint x) = x
            in if all isInt paramsNormalized
                then evalDeBruin' $ (fun $ map fromInt paramsNormalized) : overig
                else alles
evalDeBruin' other = error $ "een application van niet application-bare dingen: " ++ show other


primitives :: [(String, (Int, [Integer] -> DeBruin))]
primitives = let (-->) = (,) in
    [ "negate" --> (1, Bint . head . fmap negate)
    , "(+)" --> (2, Bint . sum)
    , "(-)" --> (2, Bint . \[a,b] -> a-b)
    , "(*)" --> (2, Bint . product)
    , "(==)" --> (2, \[a,b] -> Blambda $ Blambda $ Bvar (if a == b then 1 else 0)) -- TODO dit is nu hardcoded met de scott encoding, kijken hoe dit beter kan
    ]



-- applyToPrimitieve :: DeBruin -> [DeBruin] -> DeBruin
-- applyToPrimitieve (Bapply (Bprim "negate") (Bint x)) = Bint (-x)
-- applyToPrimitieve (Bapply (Bprim "negate") x) = applyprimitieve (Bapply (Bprim "negate") (evalDeBruin x))
-- applyToPrimitieve (Bapply (Bapply (Bprim "(+)") (Bint x)) (Bint y)) = Bint (x+y)
-- applyToPrimitieve (Bapply (Bapply (Bprim "(+)") x) y) = applyprimitieve (Bapply (Bapply (Bprim "(+)") (evalDeBruin x)) (evalDeBruin y))
-- applyToPrimitieve (Bapply (Bapply (Bprim "(-)") (Bint x)) (Bint y)) = Bint (x-y)
-- applyToPrimitieve (Bapply (Bapply (Bprim "(-)") x) y) = applyprimitieve (Bapply (Bapply (Bprim "(-)") (evalDeBruin x)) (evalDeBruin y))
-- applyToPrimitieve x = x


{-

Manieren om te evalueren
1) lambda de bruin
    a) simpel, bij apply de hele tree doorlopen en alle referenties vervangen met het geappliede
    b) sharing doen, i.e. werken met pointers (of STrefs of complexe zipper of iets in interpreter)
    c) ipv de hele tree door te lopen bij apply en alles vervangen, stack bijhouden van parameters
       indexen is minder handig te doen in haskell, maar goed.
    d) In plaats van dit, zou je niet indirect met een stack kunenn werken, maar gewoon overal direct
       pointers hebben naar een voorgemaatke thunk voor iedere lambda
    d) G machine
    e) Spineless G machine
    e) STG

    
2) Omzetten in combinators
    a) omzetten kan in meerder manieren
        i)      SKI standaard
        ii)     a la turner/microhs
        iii)    oleg
    b) interpreter
    c) c interpreter
    d) c meer?

-}