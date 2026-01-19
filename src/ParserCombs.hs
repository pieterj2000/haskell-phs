{-# LANGUAGE InstanceSigs #-}
module ParserCombs (
    Parser(..),
    -- TParser,
    token,
    ParseError(..),
    parseResult,
    -- token,
    satisfy,
    -- any,
    -- string,
    between,
    -- overrideError,
    -- many',
    --eof,
    -- tokens,
    -- tokent,
    -- stoken,
    getIf,
    someSep,
    someSep',
    manySep,
    someSep2,
) where

import ExprDef
import Control.Applicative (Alternative (..))
import Data.Traversable (traverse)
import Prelude hiding (any)
import Data.String (IsString)

import Error

-- TODO willen we een [Error] (or misschien difflist in dat geval)?
-- (Last managed position, input) -> Either (filename -> error) ((outputvalue, in position), (next managed position, rest input))
-- TODO pos verwerken
-- TODO error maken zodat pos en filename vanzelf goed gaan. (Source ipv pos)
newtype Parser i e a = Parser { runParser :: [i] -> Either e (a, [i]) }

-- TODO error gooien als niet alles geparsed is!
parseResult :: ParseError i e => Parser i e a -> [i] -> Either e a
parseResult p input = do
    (r, rest) <- runParser p input
    if null rest
        then Right r
        else Left $ unconsumedError rest

instance Functor (Parser i e) where
    fmap :: (a -> b) -> Parser i e a -> Parser i e b
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Left e -> Left e
            Right (x, rest) -> Right (f x, rest)

instance Applicative (Parser i e) where
    pure :: a -> Parser i e a
    pure x = Parser $ \input -> Right (x, input)
    (<*>) :: Parser i e (a -> b) -> Parser i e a -> Parser i e b
    af <*> ax = Parser $ \input ->
            case runParser af input of
              Left e          -> Left e
              Right (f, rest) ->
                case runParser ax rest of
                  Left e            -> Left e
                  Right (x, rest')  -> Right (f x, rest')


-- instance EmptyError ParseError where
--     emptyError :: ParseError
--     emptyError = ParseEmpty

-- instance EmptyError Error where
--     emptyError :: Error
--     emptyError = ParseError ParseEmpty (Source "ergens" 0 0)

instance ParseError i e => Alternative (Parser i e) where
    empty :: Parser i e a
    empty = Parser $ \input -> case input of
                -- [] -> Left $ emptyError -- TODO error opnieuw doen
                --                         --TODO dit is eigenlijk Empty error op EOF, dus dat is weer iets extra,
                --                                                 --weet nog steeds niet wanneer deze error precies zou moeten komen,
                --                                                 -- als het ooit nuttig is het beter om hier een losse ParseEmptyEOF te maken denk ik
                _  -> Left $ emptyError
    (<|>) :: Parser i e a -> Parser i e a -> Parser i e a
    l <|> r = Parser $ \input -> case runParser l input of
                Right x -> Right x
                Left e1  -> runParser r input


-- | expects predicate function on tokens and String describing what it expects, for error messages
satisfy :: (ParseError i e, Show i) => (i -> Bool) -> String -> Parser i e i
satisfy p = getIf (\i -> if p i then Just i else Nothing)

-- | expects function on tokens and String describing what it expects, for error messages
getIf :: (ParseError i e, Show i) => (i -> Maybe a) -> String -> Parser i e a
getIf p expects = Parser $ \input ->
        case input of 
            [] -> Left $ unexpectedError expects "end of tokens"
            (c:xs) -> case p c of
                Just a -> Right (a, xs)
                Nothing -> Left $ unexpectedError expects (show c)


-- | expects predicate function on tokens and String describing what it expects, for error messages
-- satisfy :: Show i => (i -> Bool) -> String -> Parser i ParseError i
-- satisfy p expects = Parser $ \input ->
--         case input of
--             [] -> Left $ ParseUnexpectedEOF expects
--             (c:xs) -> if p c
--                 then Right (c, xs)
--                 else Left $ ParseUnexpected (show c) expects

-- -- expects function on token and String describing what it expects, for error messages
-- satisfyMap :: (i -> Either ParseError a) -> String -> Parser i a
-- satisfyMap f expects = Parser $ \input ->
--         case input of
--             [] -> Left $ ParseError (ParseUnexpectedEOF expects) (Pos 0 0) ""
--             (c:xs) -> case f c of
--                 Right y -> Right ((y, pos), (pos, xs))
--                 Left e -> Left $ ParseError e pos


token :: (Show i, Eq i, ParseError i e) => i -> Parser i e i
token c = satisfy (==c) (show c)

-- string :: (Show i, Eq i) => [i] -> Parser i ParseError [i]
-- string = traverse char

-- any :: Parser i i
-- any = Parser $ \(rp, input) -> case input of
--         []          -> Left $ ParseError (ParseUnexpectedEOF "any symbol") rp
--         (c,pos):xs    -> Right ((c,pos), (pos, xs))

between :: Parser i e a -> Parser i e b -> Parser i e c -> Parser i e c
between l r p = l *> p <* r

-- overrideError :: Parser i a -> ParseError -> Parser i a
-- overrideError p enew = Parser $ \(rp, input) ->
--     case runParser p (rp, input) of
--         Left e ->   let (ParseError _ pos _) = e ""
--                     in Left $ ParseError enew pos  --Todo is niet type safe in dat het ook een andere error dan parseerror zou kunnen zijn, maar dat zou
--                                                                 -- niet moeten kunnen voorkomen op dit punt
--         Right x -> Right x


someSep :: ParseError i e => Parser i e a -> Parser i e b -> Parser i e [a]
someSep ding sep = (:) <$> ding <*> ( (sep *> someSep ding sep) <|> pure [] )

-- | zelfde als someSep, maar dan moeten er minstens twee zijn, dus minstens een separator
someSep2 :: ParseError i e => Parser i e a -> Parser i e b -> Parser i e [a]
someSep2 ding sep = (:) <$> ding <*> (sep *> someSep ding sep)

-- | many, maar dan geseparate door een separator. In het geval van nul of één dingen geparsed, moeten er géén seperators zijn
manySep :: ParseError i e => Parser i e a -> Parser i e b -> Parser i e [a]
manySep ding sep = someSep ding sep <|> pure []

-- | zelfde als someSep, maar dan mag de seperator meerdere keren tussen twee dingen zitten, en mag ook de 
-- input beginnen en eindigen met een of meerdere separators
someSep' :: ParseError i e => Parser i e a -> Parser i e b -> Parser i e [a]
someSep' ding sep = many sep *> someSep ding (some sep) <* many sep


-- some' :: Parser i a -> Parser i [(a, Pos)]
-- some' p = (:) <$> withPos p <*> many' p

-- many' :: Parser i a -> Parser i [(a, Pos)]
-- many' p = some' p <|> pure []

-- withPos :: Parser i a -> Parser i (a, Pos)
-- withPos p = Parser $ \input -> case runParser p input of
--                                 Left e -> Left e
--                                 Right ((a,pos), rest) -> Right (((a,pos),pos),rest)


-- eof :: Parser i (Source -> Error) ()
-- eof = Parser $ \input -> case input of
--                     [] -> Right ((), input)
--                     _ -> Left $ ParseError ParseExpectedEOF


--------------------------------------------------------------------------------------------------
-- SPECIALIZED FOR TOKENS

-- type TParser a = Parser SToken a

-- tokent :: Token -> TParser e Token
-- tokent t = fst <$> token t

-- tokens :: Token -> TParser e String
-- tokens t = snd <$> token t

-- token :: Token -> TParser e SToken
-- token t = satisfy ((==t) . fst) $ show t

-- stoken :: SToken -> TParser e SToken
-- stoken = char