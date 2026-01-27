module Parser.Fixity (
    solveFixity,
) where

import Error
import Data.Maybe (fromJust, isJust)
import Utils
import Control.Arrow (second)
import Defs.ExprDefs
import Defs.Haskell



solveFixity :: VarStore -> [HDecl] -> Either Error [HDecl]
solveFixity ctx [] = Right []
solveFixity ctx ( (HFuncDef n ps expr) : xs) = case solveFixityExpr ctx expr of 
    Left e -> Left e
    Right expr' -> (HFuncDef n ps expr' :) <$> solveFixity ctx xs
solveFixity ctx (x:xs) = (x:) <$> solveFixity ctx xs

-- solve fixity van één HExpr, dit doet hij dus recursief door de hele AST van een HExpr
solveFixityExpr :: VarStore -> HExpr -> Either Error HExpr
solveFixityExpr info (HInfixExpr xs) = solveFixityEen info xs >>= solveFixityExpr info
solveFixityExpr info (HInfixParentheses inner) = solveFixityExpr info inner
--recursieve opties
solveFixityExpr info (HApply l r) = HApply <$> solveFixityExpr info l <*> solveFixityExpr info r
solveFixityExpr info (HLambda naam def) = HLambda naam <$> solveFixityExpr info def
--overig
solveFixityExpr _ x = Right x


-- solve fixity van één HInfixExpr, en dan dus over de lijst van HExpr die daar ondefvallen
solveFixityEen :: VarStore -> [HExpr] -> Either Error HExpr
solveFixityEen info xs = case checknegatives info xs of
    Nothing -> parse info xs
    Just e -> Left e


getPrec :: VarStore -> String -> Int
getPrec = varFixityPrecedence . fromJust .: flip lookup 
getFixity :: VarStore -> String -> FixityType
getFixity = varFixity . fromJust .: flip lookup 
-- TODO default fixity. ofwel hier doen, ofwel tijdens dat de VarInfo voor ieder symbool gemaakt wordt (denk dat dat beter is?)

checknegatives :: VarStore -> [HExpr] -> Maybe Error
checknegatives _ [] = Nothing
checknegatives _ [_] = Nothing
checknegatives opinfo (HInfixOp l : HInfixOp "(-)" : rest) =
    let lprec = getPrec opinfo l
        nprec = getPrec opinfo "(-)"
    in if lprec < nprec 
        then checknegatives opinfo rest
        else Just $ fixityError
checknegatives opinfo (_:rest) = checknegatives opinfo rest

getInfixOpName :: HExpr -> Maybe String
getInfixOpName (HInfixOp x) = Just x
getInfixOpName _ = Nothing

isInfixOp :: HExpr -> Bool
isInfixOp = isJust . getInfixOpName


parse :: VarStore -> [HExpr] -> Either Error HExpr
parse info [HInfixOp "(-)", x] = Right $ HApply (HVar "negate") x
parse info [a, HInfixOp op, b] = Right $ HApply (HApply (HVar op) a) b
parse info (HInfixOp "(-)" : b : HInfixOp op : rest) =
    let p1 = getPrec info "(-)"
        t1 = getFixity info "(-)"
        p2 = getPrec info op
        t2 = getFixity info op

        sameprec = p1 == p2
        associeerbaar = t1 == t2 && (t1 == InfixL || t1 == InfixR)

        left = (t1 == InfixL && t2 == InfixL && p1 == p2) || p1 > p2

    in if sameprec && not associeerbaar
        then Left fixityError -- TODO beter error message
        else if left
            then parse info $ (HApply (HVar "negate") b) : HInfixOp op : rest
            else case parse info (b : HInfixOp op : rest) of
                Left e -> Left e
                Right hexpr -> parse info [HInfixOp "(-)", hexpr]
parse info (a : HInfixOp op : HInfixOp "(-)" : rest) = -- vanuit checknegatives weten we dat 'op' lagere precedence heeft dan (-)
            case parse info (HInfixOp "(-)" : rest) of
                Left e -> Left e
                Right hexpr -> parse info [a, HInfixOp op, hexpr]
parse info (a : HInfixOp op1 : b : HInfixOp op2 : rest) =
    let p1 = getPrec info op1
        t1 = getFixity info op1
        p2 = getPrec info op2
        t2 = getFixity info op2

        sameprec = p1 == p2
        associeerbaar = t1 == t2 && (t1 == InfixL || t1 == InfixR)

        left = (t1 == InfixL && t2 == InfixL && p1 == p2) || p1 > p2

    in if sameprec && not associeerbaar
        then Left fixityError -- TODO beter error message
        else if left
            then parse info $ (HApply (HApply (HVar op1) a) b) : HInfixOp op2 : rest
            else case parse info (b : HInfixOp op2 : rest) of
                Left e -> Left e
                Right hexpr -> parse info [a, HInfixOp op1, hexpr]


parse info x = error $ "onverwachte input bij fixity parse: " ++ show x


-- parse info spul = 
--     let (links, rest1) = break isInfixOp spul
--         op1 = fromJust . getInfixOpName $ head rest1
--         (tussen, rest2) = break isInfixOp $ tail rest1
--         op2 = fromJust . getInfixOpName $  head rest2
--         rechts = tail rest2

--         p1 = getPrec info op1
--         t1 = getFixity info op1
--         p2 = getPrec info op2
--         t2 = getFixity info op2

--         sameprec = p1 == p2
--         associeerbaar = t1 == t2 && (t1 == InfixL || t1 == InfixR)

--         klaar = null rest2

--         left = klaar || (t1 == InfixL && t2 == InfixL) || p1 > p2

--         -- (-b) = -b
--         klaarDing   | null links = HApply (HVar "negate") (head tussen)
--         -- a+b = ((+) a) b
--                     | otherwise  = HApply (HApply (HVar op1) (head links)) (head tussen)

--         -- TODO deze negate moet wss per se uit prelude komen, onafhankelijk van imports en dergelijke
--         -- (-b)+c  = (+) (-b) c = ( (+) (negate b) ) c
--         leftDing | null links   = (HApply $ HApply (HVar op2) (HApply (HVar "negate") (head tussen)) ) <$> parse info rechts
--         -- (a*b)+c  = (+) ( (*) a b) ) c = ( (+) ( (*) a b) ) ) c = ( (+) ( (* a) b) ) c
--                  | otherwise    =   (HApply $  
--                                         HApply (HVar op2) (HApply (HApply (HVar op1) (head links)) $ (head tussen))  
--                                     ) <$> parse info rechts

--         -- TODO deze negate moet wss per se uit prelude komen, onafhankelijk van imports en dergelijke
--         -- -(b+c)  = negate (b+c)
--         rightDing | null links  = HApply (HVar "negate") <$> parse info (tail rest1)
--         -- a*(b+c)  = (*) a (b+c) = ((*) a) (b+c)
--                   | otherwise   = HApply (HApply (HVar op1) (head links)) <$> parse info (tail rest1)

--     in case () of 
--         ()  | null rest1                    -> error $ "zou niet moeten gebeuren, input: " ++ show spul -- TODO in error stoppen?
--             | length links > 1              -> error "fixity: links meer dan 1 expression" -- TODO deze drie eigenlijk weghalen? Of deze functie
--             | length tussen > 1             -> error "fixity: tussen meer dan 1 expression" -- beter inrichten dat deze niet gebeurd. of error verbetern
--             | null links && op1 /= "(-)"    -> error "fixity: links geen expression, maar op1 is niet (-)"

--             | klaar                         -> Right klaarDing

--             | sameprec && not associeerbaar -> Left fixityError -- TODO beter error message

--             | left                          -> leftDing
--             | otherwise                     -> rightDing

