module LambdaCalc where -- TODO exportlijst


data LambdaCalc
    = Lvar String
    | Llambda String LambdaCalc
    | Lapply LambdaCalc LambdaCalc

data DeBruin
    = Bprim String
    | Bvar Int
    | Blambda DeBruin
    | Bapply DeBruin DeBruin

instance Show LambdaCalc where
  showsPrec p (Lvar s) = showString s
  showsPrec p (Llambda s l) = showParen (p > 0) $ showChar '\\' . showString s . showChar '.' . shows l
  showsPrec p (Lapply a b) = showParen (p > 0) $ showsPrec 1 a . showChar ' ' . showsPrec 1 b


instance Show DeBruin where
  showsPrec p (Bprim s) = (if p > 1 then (' ':) else id) . showString s
  showsPrec p (Bvar i) = showParen (i>9) (shows i)
  showsPrec p (Blambda l) = showParen (p > 0) (showChar '\\' . shows l)
  showsPrec p (Bapply a b) = showParen (p > 0) (showsPrec 1 a . showsPrec 2 b)

test = Llambda "f" $ Lapply (Llambda "x" $ Lapply (Lvar "x") (Lvar "x")) (Llambda "x" $ Lapply (Lvar "f") $ Lapply (Lvar "x") (Lvar "y"))

lambdaToDeBruin :: LambdaCalc -> DeBruin
lambdaToDeBruin = go []
    where
        go context (Lvar x) = case lookup x $ zip context [0..] of
                                    Just i -> Bvar i
                                    Nothing -> Bprim x
        go context (Llambda x y) = Blambda $ go (x:context) y
        go context (Lapply x y) = Bapply (go context x) (go context y)

-- applies b to a lambda term a. Recurses down tree and changes each instance of a with b
apply :: DeBruin -> DeBruin -> DeBruin
apply (Blambda a) b = go a 0
    where
        go (Bprim s) _      = Bprim s
        go (Bvar i) j       = if i == j then b else Bvar i
        go (Blambda l) j    = Blambda $ go l (j+1)
        go (Bapply f x) j   = Bapply (go f j) (go x j)
apply _ _ = error "applying to something different than a lambda"


-- evals de bruin naively, i.e. no sharing
evalDeBruin :: DeBruin -> DeBruin
evalDeBruin (Bapply (Blambda l) x) = evalDeBruin $ apply (Blambda l) x
evalDeBruin other = other



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