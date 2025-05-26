{-# LANGUAGE InstanceSigs #-}
module ParserCombs (
    Parser(..),
    TParser,
    char,
    parseResult,
    token,
    modidP,
    satisfy,
    any,
    string,
    between,
    overrideError
) where

import ExprDef (SToken, Pos (..), Token (..))
import Error (Error (..), ParseError)
import Control.Applicative (Alternative)
import GHC.Base (Alternative(..))
import Prelude hiding (any)

-- TOOD willen we een [Error] or misschien difflist?
newtype Parser i a = Parser { parse :: [(i, Pos)] -> Either [String -> Error] (a, [(i, Pos)]) }

parseResult :: Parser i a -> [(i, Pos)] -> Either [String -> Error] a
parseResult p = (fst <$>) . parse p

instance Functor (Parser i) where
    fmap :: (a -> b) -> Parser i a -> Parser i b
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Left e -> Left e
            Right (x, rest) -> Right (f x, rest)

instance Applicative (Parser i) where
    pure :: a -> Parser i a
    pure x = Parser $ \input -> Right (x, input)
    (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
    af <*> ax = Parser $ \input ->
            case parse af input of
              Left e          -> Left e
              Right (f, rest) ->
                case parse ax rest of
                  Left e            -> Left e
                  Right (x, rest')  -> Right (f x, rest')

instance Alternative (Parser i) where
    empty :: Parser i a
    empty = Parser $ \input -> case input of
                [] -> Left [ParseError $ ParseEmpty (Pos (-1) (-1))] --TODO dit is eigenlijk Empty error op EOF, dus dat is weer iets extra, 
                                                                --weet nog steeds niet wanneer deze error precies zou moeten komen,
                                                                -- als het ooit nuttig is het beter om hier een losse ParseEmptyEOF te maken denk ik
                (_,pos):xs  -> Left [ParseError $ ParseEmpty pos]
    (<|>) :: Parser i a -> Parser i a -> Parser i a
    l <|> r = Parser $ \input -> case parse l input of
                Right x -> Right x
                Left e1  -> case parse r input of
                    Right x -> Right x
                    Left e2 -> Left $ e1 <> e2


-- | expects predicate function on tokens and String describing what it expects, for error messages
satisfy :: Show i => (i -> Bool) -> String -> Parser i i
satisfy p expects = Parser $ \input ->
        case input of
            [] -> Left [ParseUnexpectedEOF expects]
            ((c,pos):xs) -> if p c
                then Right (c, xs)
                else Left [ParseUnexpected (show c) expects pos]

-- expects function on token and String describing what it expects, for error messages
satisfyMap :: (i -> Either (Pos -> String -> Error) a) -> String -> Parser i a
satisfyMap f expects = Parser $ \input ->
        case input of
            [] -> Left [ParseUnexpectedEOF expects]
            ((c,pos):xs) -> case f c of
                Right y -> Right (y, xs)
                Left e -> Left [e pos]


char :: (Show i, Eq i) => i -> Parser i i
char c = satisfy (==c) (show c)

string :: (Show i, Eq i) => [i] -> Parser i [i]
string = traverse char

any :: Parser i i
any = Parser $ \input -> case input of
        []          -> Left [ParseUnexpectedEOF "any symbol"]
        (c,_):xs    -> Right (c, xs)

between :: Parser i a -> Parser i b -> Parser i c -> Parser i c
between l r p = l *> p <* r

overrideError :: Parser i a -> [Pos -> String -> ParseError] -> Parser i a
overrideError p es = Parser $ \input -> 
    case parse p input of
        Left ((ParseError p s _):_) -> Left $ [ParseError p s es] -- Als empty list is dan is dit partial, maar zou nooit emptylist moeten kunnen hebben, misschien in NonEmptyList datatype veradneren?
        Right x -> Right x

--------------------------------------------------------------------------------------------------
-- SPECIALIZED FOR TOKENS

type TParser a = Parser Token a

token :: Token -> TParser Token
token = char

modidP :: TParser String
modidP = conidP

conidP :: TParser String
conidP =
    let expected = "constructor identifier"
        f (TConid s)    = Right s
        f t             = Left $ ParseUnexpected (show t) expected
    in satisfyMap f expected
