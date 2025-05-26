module Lexer (
    tokenize
) where

import ExprDef (SToken, Token (..), Pos (..))
import qualified ParserCombs as P

import Data.Char (isAlphaNum, isUpper, isLower, isDigit)
import Control.Applicative (many, Alternative ((<|>), some), optional)
import Data.Functor (($>))
import Error (Error(ParseUnexpected))

-- TODO doen!
tokenize :: String -> [SToken]
tokenize = map (\a -> (a, Pos 0 0)) . map (mapReserved . TVarid) . words


programP :: P.Parser Char [Token]
programP = many (ncommentP <|> lexemeP <|> whitespaceP <|> newlineP)

ncommentP :: P.Parser Char Token
ncommentP = (P.string "{-" $> TCommentOpen) <|> (P.string "-}" $> TCommentClose)

-- TODO rest aan toevoegen!
lexemeP :: P.Parser Char Token
lexemeP = conidP

newlineP :: P.Parser Char Token
newlineP = (P.string "\r\n" <|> P.string "\r" <|> P.string "\n" <|> P.string "\f" ) $> TNewLine

whitespaceP :: P.Parser Char Token
whitespaceP = some ((whitechar $> ()) <|> commentP) $> TWhiteSpace
    where
        whitechar = P.char '\v' <|> P.char ' ' <|> P.char '\t' -- <|> uniWhite TODO unicode encoding, here whitespace

commentP :: P.Parser Char () --TODO willen we iets met comments? Niet nodig voor parsen en runnen.....
commentP = (dashes *> optional (P.satisfy (not . isSymbol) "not a symbol" *> many anyP) *> newlineP) $> () --TODO unicode symbols ook 
    where dashes = P.string "--" *> many (P.char '-')

anyP :: P.Parser Char Char
anyP = P.overrideError (graphic <|> space <|> tab) $ [ParseUnexpected]
    where
        graphic = smallP <|> largeP <|> P.satisfy isSymbol "symbol" <|> digitP <|> P.satisfy isSpecial <|> P.char ':' <|> P.char



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


conidP :: P.Parser Char Token
conidP = (TConid <$>) $
                (:)
                    <$> largeP
                    <*> many (P.satisfy isAlphaNum "alphabetic character or digit" <|> P.char '\'')

smallP :: P.Parser Char Char
smallP = P.satisfy (\c -> isLower c || c=='_') "lowercase letter or underscore"

largeP :: P.Parser Char Char 
largeP = P.satisfy isUpper "upper case letter"

digitP :: P.Parser Char Char
digitP = P.satisfy isDigit "digit"

smalllargedigitP :: P.Parser Char Char
smalllargedigitP = P.satisfy (\c -> isUpper c || isLower c || isDigit c || c=='_') "alphabetic character or digit or underscore"


mapReserved :: Token -> Token
mapReserved (TVarid "case")     = TCase
mapReserved (TVarid "class")    = TClass
mapReserved (TVarid "data")     = TData
mapReserved (TVarid "default")  = TDefault
mapReserved (TVarid "deriving") = TDeriving
mapReserved (TVarid "do")       = TDo
mapReserved (TVarid "else")     = TElse
mapReserved (TVarid "if")       = TIf
mapReserved (TVarid "import")   = TImport
mapReserved (TVarid "in")       = TIn
mapReserved (TVarid "infix")    = TInfix
mapReserved (TVarid "infixl")   = TInfixl
mapReserved (TVarid "infixr")   = TInfixr
mapReserved (TVarid "instance") = TInstance
mapReserved (TVarid "let")      = TLet
mapReserved (TVarid "module")   = TModule
mapReserved (TVarid "newtype")  = TNewtype
mapReserved (TVarid "of")       = TOf
mapReserved (TVarid "then")     = TThen
mapReserved (TVarid "type")     = TType
mapReserved (TVarid "where")    = TWhere
mapReserved (TVarid "_")        = TUnderscore
mapReserved x                   = x