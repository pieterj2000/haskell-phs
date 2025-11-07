module LambdaCalc where -- TODO exportlijst

import ExprDef



-- declToLambda :: HDecl -> LambdaCalc
-- declToLambda (HDecl name params )


 -- TODO recursie eruithalen pass --TODO misschien beter die ergens anders neer te zetten?
astToLambdaCalc :: VarStore -> [HDecl HExpr] -> LambdaCalc
-- Er zijn geen expressions meer te converten, dus main evalueren
astToLambdaCalc ctx [] = astToLambdaCalc' ctx (HVar "main")
-- In het geval van een HDecl moeten we één voor één de parameters naar Lambda expressies vervangen.
-- Als het geen parameters meer heeft kunnen we de defintie in context opslaan
-- TODO dit is tevens meest simplisitisch, moet slimmer kunnen
astToLambdaCalc ctx (HDecl naam [] def : rest) = astToLambdaCalc (varStoreSetDef naam def ctx) rest
astToLambdaCalc ctx (HDecl naam (p:ps) def : rest) = astToLambdaCalc ctx (HDecl naam ps (HLambda p def) : rest)


astToLambdaCalc' :: VarStore -> HExpr -> LambdaCalc
astToLambdaCalc' ctx (HInt x) = Lint x 
astToLambdaCalc' ctx (HLambda naam def) = Llambda naam $ astToLambdaCalc' ctx def
-- TODO deze kijken of we die type-safe kunenn maken, ipv error
astToLambdaCalc' ctx (HInfixExpr _) = error "deze zou niet meer voor moeten komen"
astToLambdaCalc' ctx (HInfixParentheses _) = error "deze zou niet meer voor moeten komen"
astToLambdaCalc' ctx (HInfixOp _) = error "deze zou niet meer voor moeten komen"
astToLambdaCalc' ctx (HApply f x) = Lapply (astToLambdaCalc' ctx f) (astToLambdaCalc' ctx x)
-- Kijk of het een bekende variable is, en substitueer de definitie (TODO dit is de meest simplistische versie)
--TODO recursieve definities zullen nu niet goed gaan. 
-- Er moet dus vooraf een 'remove recursion' pass over de AST komen, vóór de vertaling
astToLambdaCalc' ctx (HVar x) = case lookup x ctx >>= varDefinition of 
    Nothing -> Lvar x 
    Just def -> astToLambdaCalc' ctx def





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
applyToLambda _ _ = error "applying to something different than a lambda"

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
evalDeBruin' [x] = [x] -- Dit zou niet een apply moeten kunnen zijn
evalDeBruin' (Blambda l : x : rest) = evalDeBruin' $ (applyToLambda l x) : rest
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
                then evalDeBruin' $ Bint (fun $ fromInt <$> paramsNormalized) : overig
                else alles
evalDeBruin' other = error $ "een application van niet application-bare dingen: " ++ show other


primitives :: [(String, (Int, [Integer] -> Integer))]
primitives = let (-->) = (,) in
    [ "negate" --> (1, head . fmap negate)
    , "(+)" --> (2, sum)
    , "(-)" --> (2, \[a,b] -> a-b)
    , "(*)" --> (2, product)
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