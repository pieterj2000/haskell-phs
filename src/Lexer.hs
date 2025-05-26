{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Lexer (
    tokenize
) where

import ExprDef (SToken, Token (..), Pos (..), PToken)
import qualified ParserCombs as P

import Data.Char (isAlphaNum, isUpper, isLower, isDigit)
import Control.Applicative (many, Alternative ((<|>), some), optional)
import Data.Functor (($>))
import Error (Error (..), ParseError (..))

-- TODO Pos goed doen
--tokenize :: String -> Either (String -> Error) [SToken]
tokenize x = P.parseResult programP $ map (\a -> (a, Pos 0 0)) x

programP :: P.Parser Char [PToken]
programP = let p = P.many' (ncommentP <|> lexemeP <|> whitespaceP <|> newlineP <|> ((\c -> (TTEST,[c])) <$> anyP )) -- TODO dit laatste vervangen met EOF
            in map (\(a,b) -> (mapReserved a,b)) <$> p

ncommentP :: P.Parser Char (Token, String)
ncommentP = (P.string "{-" *> ncommentfilterP) $> (TWhiteSpace, "")

ncommentfilterP :: P.Parser Char ()
ncommentfilterP = P.Parser $ \(rp,input) ->
    let go n (('{',_):('-',_):xs) = go (n+1) xs
        go n (('-',_):('}',pos):xs)
            | n > 1     = go (n-1) xs
            | n == 1    = Right (((),pos), (pos, xs))
        go n (_:xs) = go n xs
        go _ [] = Left (ParseError ParseUnclosedNComment rp)
    in go 1 input

-- TODO rest aan toevoegen!
lexemeP :: P.Parser Char (Token, String)
lexemeP = qvaridP <|> qconidP <|> specialP

specialP :: P.Parser Char (Token, String)
specialP = (\s -> (TSpecial,[s])) <$> P.satisfy isSpecial "special symbol"

qvaridP :: P.Parser Char (Token, String)
qvaridP = (\s -> (TVarid,s)) <$> ( (++) <$> (modidQP <|> pure [])  <*> (snd <$> varidP))

qconidP :: P.Parser Char (Token, String)
qconidP = (\s -> (TConid,s)) <$> ( (++) <$> (modidQP <|> pure [])  <*> (snd <$> conidP))

modidSP :: P.Parser Char [Char]
modidSP =
    let f (_,c) '.' = c <> "."
        g ss s = concat ss <> s
    in g <$> many (f <$> conidP <*> P.char '.') <*> (snd <$> conidP)

modidP :: P.Parser Char (Token, String)
modidP = (\v -> (TModid,v)) <$> modidSP

modidQP :: P.Parser Char String
modidQP = (\a _ -> a <> ".") <$> modidSP <*> P.char '.'


newlineP :: P.Parser Char (Token, String)
newlineP = (\v -> (TWhiteSpace, v)) <$>  (P.string "\r\n" <|> P.string "\r" <|> P.string "\n" <|> P.string "\f" )

whitespaceP :: P.Parser Char (Token, String)
whitespaceP = some ((whitechar $> ()) <|> commentP) $> (TWhiteSpace, "")
    where
        whitechar = P.char '\v' <|> P.char ' ' <|> P.char '\t' -- <|> uniWhite TODO unicode encoding, here whitespace

commentP :: P.Parser Char () --TODO willen we iets met comments? Niet nodig voor parsen en runnen.....
commentP = (dashes *> optional (P.satisfy (not . isSymbol) "not a symbol" *> many anyP) *> newlineP) $> () --TODO unicode symbols ook 
    where dashes = P.string "--" *> many (P.char '-')

anyP :: P.Parser Char Char
anyP = P.overrideError (graphic <|> P.char ' ' <|> P.char '\t') $ ParseUnexpected "not any" "any" -- dit is geen goede error maar past ook niet echt erin...
    where
        graphic = smallP <|> largeP <|> P.satisfy isSymbol "symbol" <|> digitP
                         <|> P.satisfy isSpecial "special symbol" <|> P.char ':'
                         <|> P.char '"' <|> P.char '\''



isSpecial :: Char -> Bool
isSpecial '('   = True
isSpecial ')'   = True
isSpecial ','   = True
isSpecial ';'   = True
isSpecial '['   = True
isSpecial ']'   = True
isSpecial '`'   = True
isSpecial '{'   = True
isSpecial '}'   = True
isSpecial _     = False


isSymbol :: Char -> Bool
isSymbol '!'    = True
isSymbol '#'    = True
isSymbol '$'    = True
isSymbol '%'    = True
isSymbol '&'    = True
isSymbol '*'    = True
isSymbol '+'    = True
isSymbol '.'    = True
isSymbol '/'    = True
isSymbol '<'    = True
isSymbol '='    = True
isSymbol '>'    = True
isSymbol '?'    = True
isSymbol '@'    = True
isSymbol '\\'   = True
isSymbol '^'    = True
isSymbol '|'    = True
isSymbol '-'    = True
isSymbol '~'    = True
isSymbol ':'    = True
isSymbol _      = False


conidP :: P.Parser Char (Token, String)
conidP = (\v -> (TConid, v)) <$> (
                (:)
                    <$> largeP
                    <*> many (P.satisfy isAlphaNum "alphabetic character or digit" <|> P.char '\'')
                )

varidP :: P.Parser Char (Token, String)
varidP = (\v -> (TVarid, v)) <$> (
                (:)
                    <$> smallP
                    <*> many (P.satisfy isAlphaNum "alphabetic character or digit" <|> P.char '\'')
                )

smallP :: P.Parser Char Char
smallP = P.satisfy (\c -> isLower c || c=='_') "lowercase letter or underscore"

largeP :: P.Parser Char Char
largeP = P.satisfy isUpper "upper case letter"

digitP :: P.Parser Char Char
digitP = P.satisfy isDigit "digit"

smalllargedigitP :: P.Parser Char Char
smalllargedigitP = P.satisfy (\c -> isUpper c || isLower c || isDigit c || c=='_') "alphabetic character or digit or underscore"


mapReserved :: (Token, String) -> (Token, String)
mapReserved (TVarid, "case")     = (TCase, "case")
mapReserved (TVarid, "class")    = (TClass, "class")
mapReserved (TVarid, "data")     = (TData, "data")
mapReserved (TVarid, "default")  = (TDefault, "default")
mapReserved (TVarid, "deriving") = (TDeriving, "deriving")
mapReserved (TVarid, "do")       = (TDo, "do")
mapReserved (TVarid, "else")     = (TElse, "else")
mapReserved (TVarid, "if")       = (TIf, "if")
mapReserved (TVarid, "import")   = (TImport, "import")
mapReserved (TVarid, "in")       = (TIn, "in")
mapReserved (TVarid, "infix")    = (TInfix, "infix")
mapReserved (TVarid, "infixl")   = (TInfixl, "infixl")
mapReserved (TVarid, "infixr")   = (TInfixr, "infixr")
mapReserved (TVarid, "instance") = (TInstance, "instance")
mapReserved (TVarid, "let")      = (TLet, "let")
mapReserved (TVarid, "module")   = (TModule, "module")
mapReserved (TVarid, "newtype")  = (TNewtype, "newtype")
mapReserved (TVarid, "of")       = (TOf, "of")
mapReserved (TVarid, "then")     = (TThen, "then")
mapReserved (TVarid, "type")     = (TType, "type")
mapReserved (TVarid, "where")    = (TWhere, "where")
mapReserved (TVarid, "_")        = (TUnderscore, "_")
mapReserved x                    = x