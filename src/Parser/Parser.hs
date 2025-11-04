{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Parser
-- TODO export list
where


import ExprDef


import qualified ParserCombs as P
import Control.Applicative (Alternative(..))
import Data.Char (isLower, isAlpha, isUpper, isDigit, digitToInt)

import Parser.Lexer
import Data.Functor (($>))
import Data.String (IsString)


-- TODO fatsoenlijk error
data Error = Error String
instance P.ParseError (Token Char) Error where
  emptyError :: Error
  emptyError = Error "error: empty alternative used"
  unexpectedError :: String -> String -> Error
  unexpectedError expected got = Error ("error, expected " ++ expected ++ ", got " ++ got ++ " instead")
  unconsumedError :: [Token Char] -> Error
  unconsumedError rest = Error $ "error, not consumed all input. Remaining: '" ++ (show rest) ++ "'"

instance Show Error where
  show :: Error -> String
  show (Error s) = s

-- addFileLocation :: String -> Error -> Error
-- addFileLocation file (Error s)


type P = P.Parser (Token Char) Error

-- TODO filename in error gooien
parseFile :: String -> String -> Either Error HExpr
parseFile filename = P.parseResult integerExpr . tokenize

-- TODO moet eigenlijk vertaald worden naar 'fromInteger 12345', of 'negate (fromInteger 12345)' als het om -x gaat
integerExpr :: P HExpr
integerExpr = HInt <$> signedinteger

signedinteger :: P Integer
signedinteger = ( ((-1)*) <$> (minus *> integer) ) <|> integer

-- TODO dit moet uiteindelijk P.symbol of zo zijn, die specifiek een symbol token eist.
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