{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Parser
-- TODO export list
where


import ExprDef


import qualified ParserCombs as P
import Control.Applicative (Alternative(..), optional)
import Data.Char (isLower, isAlpha, isUpper, isDigit, digitToInt)

import Parser.Lexer
import Data.Functor (($>))
import Parser.Fixity

import Error



type P = P.Parser (Token Char) Error

-- TODO filename in error gooien
parseFile :: String -> String -> Either Error HExpr
parseFile filename = P.parseResult infixExpression . tokenize


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
leftExpression = integerExpr

-- TODO qualified maken
infixOp :: P HExpr
infixOp = varsymbol <|> varidentInfix -- returnen allebei een HInfixOp

varidentInfix :: P HExpr
varidentInfix = 
    let f (HVar s) = HInfixOp s
        f _ = error "zou niet moeten gebeuren"
    in f <$> (P.token (Tspecialsymb '`') *> varident <* P.token (Tspecialsymb '`'))

varident :: P HExpr
varident = undefined

-- | returns the varsymbol already enclosed in braces, i.e. reads ++ as (++)
varsymbol :: P HExpr
varsymbol = P.getIf (\t -> case t of
    (Tsymbols s) -> if isReservedOp s then Nothing else Just $ HInfixOp $ "(" ++ s ++ ")"
    _ -> Nothing) "(infix) operator consisting of symbols"




-- TODO moet eigenlijk vertaald worden naar 'fromInteger 12345', of 'negate (fromInteger 12345)' als het om -x gaat
integerExpr :: P HExpr
integerExpr = HInt <$> integer

signedinteger :: P Integer
signedinteger = ( ((-1)*) <$> (minus *> integer) ) <|> integer


unitaryMinusOp :: P HExpr
unitaryMinusOp = minus *> ( prependInfixOp <$> infixExpression)
    where 
        prependInfixOp :: HExpr -> HExpr
        prependInfixOp (HInfixExpr rest) = HInfixExpr $ (HInfixOp "(-)") : rest
        prependInfixOp anders = HInfixExpr $ [HInfixOp "(-)", anders]

-- TODO weghalen, en alleen na de fixitycheck pas uitten...
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

