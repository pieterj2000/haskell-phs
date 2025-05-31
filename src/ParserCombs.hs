{-# LANGUAGE InstanceSigs #-}
module ParserCombs (
    Parser(..),
    TParser,
    char,
    parseResult,
    token,
    satisfy,
    any,
    string,
    between,
    overrideError,
    many',
    eof,
    tokens,
    tokent,
    stoken
) where

import ExprDef (SToken, PToken, Pos (..), Token (..))
import Error (Error (..), ParseError (..))
import Control.Applicative (Alternative)
import GHC.Base (Alternative(..))
import Prelude hiding (any)

-- TODO willen we een [Error] (or misschien difflist in dat geval)?
-- (Last managed position, input) -> Either (filename -> error) ((outputvalue, in position), (next managed position, rest input))
newtype Parser i a = Parser { runParser :: (Pos, [(i, Pos)]) -> Either (String -> Error) ((a, Pos), (Pos, [(i, Pos)])) }

parse :: Parser i a -> [(i, Pos)] -> Either (String -> Error) ((a, Pos), (Pos, [(i, Pos)]))
parse p input = runParser p (Pos 1 1, input)

parseResult :: Parser i a -> [(i, Pos)] -> Either (String -> Error) a
parseResult p = (fst <$> fst <$>) . parse p

instance Functor (Parser i) where
    fmap :: (a -> b) -> Parser i a -> Parser i b
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Left e -> Left e
            Right ((x, pos), rest) -> Right ((f x, pos), rest)

instance Applicative (Parser i) where
    pure :: a -> Parser i a
    pure x = Parser $ \(rp, input) -> case input of
                            []          -> Right ((x,rp), (rp,input))
                            (_,np):_    -> Right ((x,np), (rp,input)) -- When you do (pure [] <*>) for example with an optional qualified
                                                                      -- then needs to take the pos of the next char, otherwise the token
                                                                      -- gets the value of one pos before the token  
                                                                      -- maybe this goes wrong somewhere if you do <*> pure 
                                                                      -- we'll see it when it happens....
    (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
    af <*> ax = Parser $ \input ->
            case runParser af input of
              Left e          -> Left e
              Right ((f,pos), rest) ->
                case runParser ax rest of
                  Left e            -> Left e
                  Right ((x, _), rest')  -> Right ((f x, pos), rest')

instance Alternative (Parser i) where
    empty :: Parser i a
    empty = Parser $ \(rp, input) -> case input of
                [] -> Left $ ParseError ParseEmpty rp --TODO dit is eigenlijk Empty error op EOF, dus dat is weer iets extra, 
                                                                --weet nog steeds niet wanneer deze error precies zou moeten komen,
                                                                -- als het ooit nuttig is het beter om hier een losse ParseEmptyEOF te maken denk ik
                _  -> Left $ ParseError ParseEmpty rp
    (<|>) :: Parser i a -> Parser i a -> Parser i a
    l <|> r = Parser $ \input -> case runParser l input of
                Right x -> Right x
                Left e1  -> runParser r input


-- | expects predicate function on tokens and String describing what it expects, for error messages
satisfy :: Show i => (i -> Bool) -> String -> Parser i i
satisfy p expects = Parser $ \(rp, input) ->
        case input of
            [] -> Left $ ParseError (ParseUnexpectedEOF expects) rp
            ((c,pos):xs) -> if p c
                then Right ((c,pos), (pos, xs))
                else Left $ ParseError (ParseUnexpected (show c) expects) pos

-- expects function on token and String describing what it expects, for error messages
satisfyMap :: (i -> Either ParseError a) -> String -> Parser i a
satisfyMap f expects = Parser $ \(rp, input) ->
        case input of
            [] -> Left $ ParseError (ParseUnexpectedEOF expects) rp
            ((c,pos):xs) -> case f c of
                Right y -> Right ((y, pos), (pos, xs))
                Left e -> Left $ ParseError e pos


char :: (Show i, Eq i) => i -> Parser i i
char c = satisfy (==c) (show c)

string :: (Show i, Eq i) => [i] -> Parser i [i]
string = traverse char

any :: Parser i i
any = Parser $ \(rp, input) -> case input of
        []          -> Left $ ParseError (ParseUnexpectedEOF "any symbol") rp
        (c,pos):xs    -> Right ((c,pos), (pos, xs))

between :: Parser i a -> Parser i b -> Parser i c -> Parser i c
between l r p = l *> p <* r

overrideError :: Parser i a -> ParseError -> Parser i a
overrideError p enew = Parser $ \(rp, input) ->
    case runParser p (rp, input) of
        Left e ->   let (ParseError _ pos _) = e ""
                    in Left $ ParseError enew pos  --Todo is niet type safe in dat het ook een andere error dan parseerror zou kunnen zijn, maar dat zou
                                                                -- niet moeten kunnen voorkomen op dit punt
        Right x -> Right x


some' :: Parser i a -> Parser i [(a, Pos)]
some' p = (:) <$> withPos p <*> many' p

many' :: Parser i a -> Parser i [(a, Pos)]
many' p = some' p <|> pure []

withPos :: Parser i a -> Parser i (a, Pos)
withPos p = Parser $ \input -> case runParser p input of
                                Left e -> Left e
                                Right ((a,pos), rest) -> Right (((a,pos),pos),rest)


eof :: Parser i ()
eof = Parser $ \(rp,input) -> case input of
                    [] -> Right (((),rp), (rp,input))
                    _ -> Left $ ParseError ParseExpectedEOF rp


--------------------------------------------------------------------------------------------------
-- SPECIALIZED FOR TOKENS

type TParser a = Parser SToken a

tokent :: Token -> TParser Token
tokent t = fst <$> token t

tokens :: Token -> TParser String
tokens t = snd <$> token t

token :: Token -> TParser SToken
token t = satisfy ((==t) . fst) $ show t

stoken :: SToken -> TParser SToken
stoken = char