{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Parser
(
 parseFile
)
-- TODO export list
where


import Defs.Haskell


import qualified ParserCombs as P
import Control.Applicative (Alternative(..), optional)
import Data.Char (isLower, isAlpha, isUpper, isDigit, digitToInt)

import Parser.Lexer
import Data.Functor (($>))
import Parser.Fixity

import Error
import Data.List (singleton, foldl1')
import Defs.Common (Type(..))








type P = P.Parser (Token Char) Error

-- TODO filename in error gooien
parseFile :: String -> String -> Either Error [HDecl]
parseFile filename = P.parseResult topdecls . tokenize


topdecls :: P [HDecl] 
topdecls = P.someSep' topdecl (P.token (Tspecialsymb ';'))

topdecl :: P (HDecl)
topdecl = datadecl <|> typesignature <|> decl




------------------------------------------------------------------
------ TYPE SIGNATURE
------------------------------------------------------------------

-- TODO al deze onderstaande afmaken
-- TODO er moet achteraf een check komen of alle arities/kinds wel kloppen van alle data constructors (en van () [] (->) (,) (,,) etc ) dus of het een kloppende type is...
typesignature :: P HDecl
typesignature = HTypeSig <$> (varidentString <* P.token (Tsymbols "::")) <*> typedeel

typedeel :: P Type
typedeel =
    let f t Nothing = t
        f t (Just x) = TypeArrow t x
    in f <$> typedeelb <*> optional (P.token (Tsymbols "->") *> typedeel)

typedeelb :: P Type
typedeelb = foldl1' TypeApply <$> some typedeela

typedeela :: P Type
typedeela = typedeelconstructor 
        <|> ( TypeVar <$> varidentString )
        <|> typedeela_tuple
        <|> ( (TypeApply (TypeConstr "[]")) <$> (P.between (P.token $ Tspecialsymb '[') (P.token $ Tspecialsymb ']') typedeel) )
        <|> ( P.between (P.token $ Tspecialsymb '(') (P.token $ Tspecialsymb ')') typedeel )

typedeela_tuple :: P Type
typedeela_tuple =
    let cons x = "(" ++ replicate (length x - 1) ',' ++ ")"
        f x = foldl' TypeApply (TypeConstr $ cons x) x
        -- TODO: deze between haakjes moet gewoon een eigen functie zijn, komt vast vaker voor...
    in f <$> P.between (P.token $ Tspecialsymb '(') (P.token $ Tspecialsymb ')') ( P.someSep2 typedeel (P.token $ Tspecialsymb ',') )

typedeelconstructor :: P Type
typedeelconstructor = (TypeConstr <$> constructorIdentifierString)










decl :: P HDecl
decl =
    let f [x] e = HFuncDef x [] e
        f (y:ys) e = HFuncDef y ys e
    in f <$> some varidentString <*> (P.token (Tsymbols "=") *> infixExpression) 


-- TODO context (dus zoals data 'Eq a => Set a = ....')
-- TODO deriving
datadecl :: P HDecl
datadecl = 
    let f naam params Nothing = HDataDef $ DataDef naam params []
        f naam params (Just cons) = HDataDef $ DataDef naam params cons

    in f <$> ((P.token (Treserved "data")) *> constructorIdentifierString) <*> many varidentString <*> (optional $ P.token (Tsymbols "=") *> P.someSep constr (P.token $ Tsymbols "|"))

-- TODO: stricte fields
-- TODO: Field labels (i.e. { x :: a, y :: b }) notatie
-- TODO types ook lezen
constr :: P DataConsDef
constr = DataConsDef <$> constructorIdentifierString <*> pure 0

expression :: P HExpr
expression = infixExpression

infixExpression :: P HExpr
infixExpression 
    = (combineInfixExpr <$> leftExpression <*> infixOp <*> infixExpression)
    <|> unitaryMinusOp -- Deze returnt uit zichzelf al een HInfixExpr ['-', ....]
    <|> leftExpression -- Deze returnt gewoon een fatsoenlijke lexpr

combineInfixExpr :: HExpr -> HExpr -> HExpr -> HExpr
combineInfixExpr lexpr iop@(HInfixOp op) (HInfixExpr rest) = HInfixExpr $ lexpr : iop : rest
combineInfixExpr lexpr iop@(HInfixOp op) anders = HInfixExpr $ [lexpr, iop, anders]
combineInfixExpr lexpr _ _ = error "combineInfixExpr, krijg andere parsings dan HInfixOp"

    -- let f l Nothing = l
    --     f l (Just (op, r)) = HInfixExpr l op r
    -- in f <$> noInfixExpression <*> optional ((,) <$> infixOp <*> infixExpression)
--DIT HIERBOVEN IS OUD, DIT HIERONDER WAS EEN POGING OM HET TE OPTIMALISEREN (AANGEZIEN LEXPR GEDEELD WORDT TUSSEN DE EERSTE EN DERDE OPTIE)
-- infixExpression = P.Parser $ \input -> 
    -- let lexprE = P.runParser leftExpression input
    --     opE = (P.runParser infixOp . snd) =<< lexprE
    --     rexprE = (P.runParser infixOp . snd) =<< lexprE
    -- in case rexprE of
    --     Right x -> Right x
    --     _       -> 


leftExpression :: P HExpr
leftExpression
    = caseExpression
    <|> functionapplicationexpression






------------------------------------------------------------------
------ CASE EXPRESSIONS
------------------------------------------------------------------

caseExpression :: P HExpr
caseExpression = 
    let f e alts = HCase e alts
    in f <$> (P.token (Treserved "case") *> expression <* P.token (Treserved "of")) 
        <*> (P.token (Tspecialsymb '{') *> P.someSep caseAlt (P.token (Tspecialsymb ';')) <* P.token (Tspecialsymb '}'))

caseAlt :: P (HPattern, HExpr)
caseAlt = (,) <$> (patternParser <* P.token (Tsymbols "->"))
            <*> expression





------------------------------------------------------------------
------ PATTERNS
------------------------------------------------------------------

-- TODO infix constructors ook
patternParser :: P HPattern
patternParser = lpatternParser

lpatternParser :: P HPattern
lpatternParser = apatternParser
                <|> minus *> ( (HPIntLiteral . negate) <$> integer) -- TODO ook float literal 
                <|> HPCons <$> constructorIdentifierString <*> some apatternParser

-- TODO hier ook de andere opties allemaal
apatternParser :: P HPattern
apatternParser = HPCons <$> constructorIdentifierString <*> pure []








functionapplicationexpression :: P HExpr
functionapplicationexpression = 
    let f [x] = x
        f (x:y:xs) = f $ (HApply x y) : xs
        f [] = error "zou niet moeten kunnen" 
    in f <$> some algemeneexpression

algemeneexpression :: P HExpr
algemeneexpression
    =   integerExpr
    <|> varident
    <|> constructorIdentifier
    <|> ( HInfixParentheses <$> P.between (P.token $ Tspecialsymb '(') (P.token $ Tspecialsymb ')') infixExpression )


-- TODO qualified maken
infixOp :: P HExpr
infixOp = varsymbol <|> varidentInfix <|> conssymbol <|> constructorIdentifierInfix -- returnen allemaal een HInfixOp

varidentInfix :: P HExpr
varidentInfix = 
    let f (HVar s) = HInfixOp s
        f _ = error "zou niet moeten gebeuren"
    in f <$> (P.token (Tspecialsymb '`') *> varident <* P.token (Tspecialsymb '`'))

-- TODO qualified maken
varident :: P HExpr
varident = HVar <$> varidentString

varidentString :: P String
varidentString = P.getIf (\t -> case t of
    (Tvarid s) -> if isReservedOp s then Nothing else Just s --TODO deze check zit misschien ook al in de lexer
                                    -- kijk wat we er mee willen, als we het hier houden moeten we opletten dat de volgorde van
                                    -- de parsers goed is, en als we het in de parser houden moeten we een extra lexemetype ondersteunen
    _ -> Nothing) "varid"

-- | returns the varsymbol already enclosed in braces, i.e. reads ++ as (++)
varsymbol :: P HExpr
varsymbol = P.getIf (\t -> case t of
    (Tsymbols s) -> if isReservedOp s then Nothing else Just $ HInfixOp $ "(" ++ s ++ ")"
    _ -> Nothing) "(infix) operator consisting of symbols"

-- TODO qualified maken
constructorIdentifier :: P HExpr
constructorIdentifier = HDataConstructor <$> constructorIdentifierString

constructorIdentifierString :: P String
constructorIdentifierString = P.getIf (\t -> case t of
    (Tconsid s) -> Just s --TODO deze check zit misschien ook al in de lexer
                                    -- kijk wat we er mee willen, als we het hier houden moeten we opletten dat de volgorde van
                                    -- de parsers goed is, en als we het in de parser houden moeten we een extra lexemetype ondersteunen
    _ -> Nothing) "conid"

-- | returns the conssymbol already enclosed in braces, i.e. reads :++ as (:++)
conssymbol :: P HExpr
conssymbol = P.getIf (\t -> case t of
    -- de enige reserved symbol die met : begint (behalve :, maar dat is wel een data constructor) is ::
    (Tsymbols s@(':' : _)) -> if s == "::" then Nothing else Just $ HInfixOp $ "(" ++ s ++ ")"
    _ -> Nothing) "(infix) data constructor consisting of symbols"

constructorIdentifierInfix :: P HExpr
constructorIdentifierInfix = 
    let f (HDataConstructor s) = HInfixOp s
        f _ = error "zou niet moeten gebeuren"
    in f <$> (P.token (Tspecialsymb '`') *> constructorIdentifier <* P.token (Tspecialsymb '`'))





-- TODO moet eigenlijk vertaald worden naar 'fromInteger 12345', of 'negate (fromInteger 12345)' als het om -x gaat
integerExpr :: P HExpr
integerExpr = HInt <$> integer


unitaryMinusOp :: P HExpr
unitaryMinusOp = minus *> ( prependInfixOp <$> infixExpression)
    where 
        prependInfixOp :: HExpr -> HExpr
        prependInfixOp (HInfixExpr rest) = HInfixExpr $ (HInfixOp "(-)") : rest
        prependInfixOp anders = HInfixExpr $ [HInfixOp "(-)", anders]

minus :: P ()
minus = P.token (Tsymbols "-") $> ()

integer :: P Integer
integer = P.getIf (\t -> case t of
                            (Tinteger x) -> Just x
                            _ -> Nothing
                    ) "integer token"


-- -- TODO, lhs moet beter worden gedaan
-- -- TODO rhs beters
-- declaration :: P.Parser SString Error HDecl
-- declaration = HDecl <$> (val <$> tvarid) <*> many (val <$>tvarid) <*> (token (P.char '=') *> pure []) <*> (HExpr . val <$> tvarid)

-- lower :: P.Parser Char ParseError Char
-- lower = P.satisfy isLower "lower case letter"

-- upper :: P.Parser Char ParseError Char
-- upper = P.satisfy isUpper "upper case letter"

-- digit :: P.Parser Char ParseError Char
-- digit = P.satisfy isDigit "digit"

-- underscore :: P.Parser Char ParseError Char
-- underscore = P.char '_'

-- varid :: P.Parser Char ParseError String
-- varid = (:) <$> (lower <|> underscore) <*> many (lower <|> upper <|> digit <|> P.char '\'')

-- tvarid :: P.Parser SString Error (WithSource String)
-- tvarid = token varid

-- --TODO naar lexer?
-- token :: P.Parser Char ParseError a -> P.Parser SString Error (WithSource a)
-- token p = P.Parser $ \input -> case input of
--         [] -> Left $ ParseError (ParseUnexpectedEOF "token") (Source "" 0 0)
--         (s:ss) -> case P.runParser p (val s) of
--             Left e -> Left $ ParseError e (source s)
--             Right (x, rest) -> case rest of
--                 [] -> Right (WithSource x (source s), ss)
--                 _  -> Left $ ParseError (ParseUnexpected "part of token remaining" "token not fully consumed") $ source s




isReservedOp :: String -> Bool
isReservedOp ".."   = True
isReservedOp ":"   = True
isReservedOp "::"   = True
isReservedOp "="   = True
isReservedOp "\\"   = True
isReservedOp "|"   = True
isReservedOp "<-"   = True
isReservedOp "->"   = True
isReservedOp "@"   = True
isReservedOp "~"   = True
isReservedOp "=>"   = True
isReservedOp _      = False

