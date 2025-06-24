module Parser.Parser
(
parseFile) where

import Error (Error (..), ParseError (..))
import ExprDef


import qualified ParserCombs as P
import Control.Applicative (Alternative(..))
import Data.Char (isLower, isAlpha, isUpper, isDigit)

import Parser.Lexer

data HExpr
    = HExpr String
    deriving (Show)

data HDecl
    = HDecl String [String] [HExpr] HExpr -- name, parameters, TODO guards, definition -- TODO dit moet lazy binding zijn? nog uitzoeken/doen
    deriving (Show)


parseFile :: String -> String -> Either Error HDecl
parseFile filename input = fst <$> runParserLex declaration filename input

-- TODO, lhs moet beter worden gedaan
-- TODO rhs beters
declaration :: P.Parser SString Error HDecl
declaration = HDecl <$> (val <$> tvarid) <*> many (val <$>tvarid) <*> (token (P.char '=') *> pure []) <*> (HExpr . val <$> tvarid)

lower :: P.Parser Char ParseError Char
lower = P.satisfy isLower "lower case letter"

upper :: P.Parser Char ParseError Char
upper = P.satisfy isUpper "upper case letter"

digit :: P.Parser Char ParseError Char
digit = P.satisfy isDigit "digit"

underscore :: P.Parser Char ParseError Char
underscore = P.char '_'

varid :: P.Parser Char ParseError String
varid = (:) <$> (lower <|> underscore) <*> many (lower <|> upper <|> digit <|> P.char '\'')

tvarid :: P.Parser SString Error (WithSource String)
tvarid = token varid


token :: P.Parser Char ParseError a -> P.Parser SString Error (WithSource a)
token p = P.Parser $ \input -> case input of 
        [] -> Left $ ParseError (ParseUnexpectedEOF "token") (Source "" 0 0)
        (s:ss) -> case P.runParser p (val s) of
            Left e -> Left $ ParseError e (source s)
            Right (x, rest) -> case rest of
                [] -> Right (WithSource x (source s), ss)
                _  -> Left $ ParseError (ParseUnexpected "part of token remaining" "token not fully consumed") $ source s