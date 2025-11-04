module Parser.Lexer (
    tokenize,
    -- withpos --todo deze eruit
-- , qvarTP
-- , conTP
-- , varTP
--  runParserLex,
--  SString) where
    Token(..)
) where

import ExprDef
import qualified ParserCombs as P

import Data.Char (isAlphaNum, isUpper, isLower, isDigit, digitToInt)
import Control.Applicative (many, Alternative ((<|>), some), optional)
import Data.Functor (($>))
import Error ( ParseError (..), Error (ParseError))
import Utils




data Token i
    = Tsymbols [i]
    | Tinteger Integer
    deriving (Show, Eq)

tokenize :: String -> [Token Char]
tokenize [] = []
tokenize spul | isDigit (head spul) = let (digits, rest) = span isDigit spul in Tinteger (digitsToInt digits) : tokenize rest
tokenize spul | isWhiteChar (head spul) = tokenize $ dropWhile isWhiteChar spul
tokenize spul = [Tsymbols $ "error: resterende tokens die we niet snappen: '" ++ spul ++ "'"] -- TODO hier fatsoenlijk error maken? Of alleen error (dus crash)


digitsToInt :: String -> Integer
digitsToInt = foldl' (\acc el -> acc*10 + toInteger el) 0 . map digitToInt


isWhiteChar :: Char -> Bool
isWhiteChar '\n' = True
isWhiteChar '\v' = True
isWhiteChar ' '  = True
isWhiteChar '\t' = True
isWhiteChar '\r' = True
isWhiteChar '\f' = True
isWhiteChar _ = False
















-- -- TODO layout
-- -- TODO pos
-- runParserLex :: P.Parser SString Error a -> String -> String -> Either Error (a, [SString])
-- runParserLex p filename input = tokenize filename input >>= runParserLex' p filename

-- runParserLex' :: P.Parser SString Error a -> String -> [SString] -> Either Error (a, [SString])
-- runParserLex' p filename [] = P.runParser p []
-- runParserLex' p filename (s:ss) =
--             let p' = P.Parser $ \i -> P.runParser p (s:i)
--             in case runParserLex' p' filename ss of
--                 Right x -> Right x
--                 Left e -> if True -- TODO layout
--                     then runParserLex' p' filename ((WithSource "}" (source s)):ss)
--                     else case e of --Dit is niet de meest elegante manier om hier source van in te bakken, maar afijn
--                         ParseError (ParseUnexpectedEOF expect) _ -> Left $ ParseError (ParseUnexpectedEOF expect) (source s)
--                         ParseError ParseEmpty _ -> Left $ ParseError ParseEmpty (source s)
--                         x -> Left x

-- type SString = WithSource String


-- tokenize :: String -> String -> Either Error [SString]
-- tokenize filename = fmap (split . commentfilter) . ncommentfilter . withpos filename


-- split :: [WithSource Char] -> [WithSource String]
-- split input = 
--     let isding c = c == '\'' || c == '"'
--         (eerst, rest) = break (isding . val) input
--         eerst' = splitOnWhite eerst
--     in if null rest 
--         then eerst'
--         else eerst' ++ case head rest of -- TODO difference list gebruiken
--             (WithSource '\'' s) -> splitChar rest
--             (WithSource '"' s) -> splitString rest
--             _ -> error "Error in Lexer.split: weird case option. This should not happen"

-- splitChar :: [WithSource Char] -> [WithSource String]
-- splitChar spul = let (chars, rest) = go $ tail spul in sCharConcat (head spul : chars) : rest
--     where
--         go (a:b:xs) | val a == '\\', val b == '\''  = let (chars, rest) = go xs in (a:b:chars, rest)
--         go (a:xs)   | val a == '\''                 = ([a], split xs)
--         go (a:xs)                                   = let (chars, rest) = go xs in (a:chars, rest)
--         go []                                       = ([], [])

-- splitString :: [WithSource Char] -> [WithSource String]
-- splitString spul = let (chars, rest) = go $ tail spul in sCharConcat (head spul : chars) : rest
--     where
--         go (a:b:xs) | val a == '\\', val b == '\"'          = let (chars, rest) = go xs in (a:b:chars, rest)
--         go (a:b:xs) | val a == '\\', isWhiteChar (val b)    = 
--             let skipwhite = dropWhile (isWhiteChar . val) xs
--                 skipwhite' = if null skipwhite then skipwhite else tail skipwhite -- afsluitende \ ook weghalen
--             in go skipwhite'
--         go (a:b:xs) | val a == '\\'                         = let (chars, rest) = go xs in (a:b:chars, rest)
--         go (a:xs)   | val a == '\"'                         = ([a], split xs)
--         go (a:xs)                                           = let (chars, rest) = go xs in (a:chars, rest)
--         go []                                               = ([], [])
        


-- sCharConcat :: [WithSource Char] -> WithSource String
-- sCharConcat [] = error "sCharConcat with empty list. This should not happen"
-- sCharConcat spul = WithSource (map val spul) (source $ head spul)


-- --TODO whitespace in strings nog, mss in tokenize een losse dinges erin doen (na comments, voor splitonwhite)
-- splitOnWhite :: [WithSource Char] -> [WithSource String]
-- splitOnWhite input =
--     let spul = dropWhile (isWhiteChar . val) input
--         (token, rest) = break (isWhiteChar . val) spul
--         token' = sCharConcat token
--     in if null spul then [] else token' : splitOnWhite rest

-- withpos :: String -> String -> [WithSource Char]
-- withpos filename = map (\(c,line,col) -> WithSource c $ Source filename line col) . go 1 1
--     where
--         go :: Int -> Int -> String -> [(Char, Int, Int)]
--         go line col ('\r':'\n':ss) = ('\r', line, col) : ('\n', line, col) : go (line+1) 1 ss
--         go line col ('\r':ss) = ('\r', line, col) : go (line+1) 1 ss
--         go line col ('\n':ss) = ('\n', line, col) : go (line+1) 1 ss
--         go line col ('\f':ss) = ('\f', line, col) : go (line+1) 1 ss
--         go line col ('\t':ss) =
--             let r = col `rem` 8
--                 extra = 8 - r
--             in ('\t', line, col+extra) : go line (col+1+extra) ss
--         go line col (c:ss) = (c,line, col) : go line (col+1) ss
--         go line col [] = []

-- commentfilter :: [WithSource Char] -> [WithSource Char]
-- commentfilter (a:b:c:xs)
--     | val a == '-', val b == '-', (not . isSymbol $ val c) = commentfilter rest
--         where
--             l = line . source $ a
--             rest = dropWhile ((==l) . line . source) xs
-- commentfilter (x:xs) = x : commentfilter xs
-- commentfilter [] = []

-- ncommentfilter :: [WithSource Char] -> Either Error [WithSource Char]
-- ncommentfilter input =
--     let go :: Source -> Int -> [WithSource Char] -> Either Error [WithSource Char]
--         go s n (a:b:c:xs) | val a == '{', val b == '-', (val c /= '#' || n > 0) = go (if n > 0 then s else source a) (n+1) (c:xs)
--         go s n (a:b:xs) | val a == '-', val b == '}' =
--             if n == 1
--                 then Right xs
--                 else if n > 1
--                     then go s (n-1) xs
--                     else error "During ncommentfilter go value n<1. This should not happen."
--         go s n (_:xs) = go s n xs
--         go s n []
--             | n > 0 = Left (ParseError ParseUnclosedNComment s)
--             | otherwise = Right input
--     in go (Source "" 0 0) 0 input











































--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------
--------------------------------------











-- withpos :: String -> [(Char, Pos)]
-- withpos = go (Pos { line = 1, col = 1 })
--     where
--         go p@(Pos line col) ('\r':'\n':ss) = ('\r',p) : ('\n',p) : go (Pos (line+1) 1) ss
--         go p@(Pos line col) ('\r':ss) = ('\r',p) : go (Pos (line+1) 1) ss
--         go p@(Pos line col) ('\n':ss) = ('\n',p) : go (Pos (line+1) 1) ss
--         go p@(Pos line col) ('\f':ss) = ('\f',p) : go (Pos (line+1) 1) ss
--         go p@(Pos line col) ('\t':ss) =
--             let r = col `rem` 8
--                 extra = 8 - r
--                 p' = Pos line (col + extra)
--             in ('\t',p') : go (Pos line (col+1+extra)) ss
--         go p@(Pos line col) (c:ss) = (c,p) : go (Pos line (col+1)) ss
--         go p@(Pos line col) [] = []


--tokenize :: String -> Either (String -> Error) [SToken]
-- tokenize x = P.parseResult programP $ withpos x

--TODO voor pragmas, als allereerste parser een pragma parser ertussen stoppen (dus between {-# en #-})
-- programP :: P.Parser Char [PToken]
-- programP = let p = P.many' (ncommentP <|> whitespaceP <|> lexemeP <|> newlineP <|> ((\c -> (TTEST,[c])) <$> anyP )) -- TODO dit laatste vervangen met EOF
--             in map (\(a,b) -> (mapReserved a,b)) <$> p

-- TODO literate haskel
-- ncommentP :: P.Parser Char (Token, String)
-- ncommentP = (P.string "{-" *> ncommentfilterP) $> (TWhiteSpace, "")

-- ncommentfilterP :: P.Parser Char ()
-- ncommentfilterP = P.Parser $ \(rp,input) ->
--     let go n (('{',_):('-',_):xs) = go (n+1) xs
--         go n (('-',_):('}',pos):xs)
--             | n > 1     = go (n-1) xs
--             | n == 1    = Right (((),pos), (pos, xs))
--         go n (_:xs) = go n xs
--         go _ [] = Left (ParseError ParseUnclosedNComment rp)
--     in go 1 input

-- TODO rest aan toevoegen!
-- lexemeP :: P.Parser Char (Token, String)
-- lexemeP = qvaridP <|> qconidP <|> qvarsymP <|> specialP

-- specialP :: P.Parser Char (Token, String)
-- specialP = (\s -> (TSpecial,[s])) <$> P.satisfy isSpecial "special symbol"

-- qvaridP :: P.Parser Char (Token, String)
-- qvaridP = (\s -> (TVarid,s)) <$> ( (++) <$> (modidQP <|> pure [])  <*> (snd <$> varidP))

-- qconidP :: P.Parser Char (Token, String)
-- qconidP = (\s -> (TConid,s)) <$> ( (++) <$> (modidQP <|> pure [])  <*> (snd <$> conidP))

-- qvarsymP :: P.Parser Char (Token, String)
-- qvarsymP = (\s1 (t,s2) -> (t,s1++s2)) <$> (modidQP <|> pure [])  <*> varsymP

-- modidSP :: P.Parser Char [Char]
-- modidSP =
--     let f (_,c) '.' = c <> "."
--         g ss s = concat ss <> s
--     in g <$> many (f <$> conidP <*> P.char '.') <*> (snd <$> conidP)

-- modidP :: P.Parser Char (Token, String)
-- modidP = (\v -> (TModid,v)) <$> modidSP

-- modidQP :: P.Parser Char String
-- modidQP = (\a _ -> a <> ".") <$> modidSP <*> P.char '.'


-- newlineP :: P.Parser Char (Token, String)
-- newlineP = (\v -> (TWhiteSpace, v)) <$>  (P.string "\r\n" <|> P.string "\r" <|> P.string "\n" <|> P.string "\f" )

-- whitespaceP :: P.Parser Char (Token, String)
-- whitespaceP = some ((whitechar $> ()) <|> commentP) $> (TWhiteSpace, "")
--     where
--         whitechar = P.char '\v' <|> P.char ' ' <|> P.char '\t' -- <|> uniWhite TODO unicode encoding, here whitespace

-- commentP :: P.Parser Char () --TODO willen we iets met comments? Niet nodig voor parsen en runnen.....
-- commentP = (dashes *> optional (P.satisfy (not . isSymbol) "not a symbol" *> many anyP) *> newlineP) $> () --TODO unicode symbols ook 
--     where dashes = P.string "--" *> many (P.char '-')

-- anyP :: P.Parser Char Char
-- anyP = P.overrideError (graphic <|> P.char ' ' <|> P.char '\t') $ ParseUnexpected "not any" "any" -- dit is geen goede error maar past ook niet echt erin...
--     where
--         graphic = smallP <|> largeP <|> symbolP <|> digitP
--                          <|> P.satisfy isSpecial "special symbol" <|> P.char ':'
--                          <|> P.char '"' <|> P.char '\''




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
isSymbol _      = False

-- symbolP :: P.Parser Char Char
-- symbolP = P.satisfy isSymbol "symbol"

-- conidP :: P.Parser Char (Token, String)
-- conidP = (\v -> (TConid, v)) <$> (
--                 (:)
--                     <$> largeP
--                     <*> many (P.satisfy isAlphaNum "alphabetic character or digit" <|> P.char '\'')
--                 )

-- varidP :: P.Parser Char (Token, String)
-- varidP = (\v -> (TVarid, v)) <$> (
--                 (:)
--                     <$> smallP
--                     <*> many (P.satisfy isAlphaNum "alphabetic character or digit" <|> P.char '\'')
--                 )

-- varsymP :: P.Parser Char (Token, String)
-- varsymP = (\v -> if isReservedOp v then (TSpecialOp,v) else (TVarsym, v)) <$> (
--                 (:)
--                     <$> symbolP
--                     <*> many (symbolP <|> P.char ':')
--                 )

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



-- smallP :: P.Parser Char Char
-- smallP = P.satisfy (\c -> isLower c || c=='_') "lowercase letter or underscore"

-- largeP :: P.Parser Char Char
-- largeP = P.satisfy isUpper "upper case letter"

-- digitP :: P.Parser Char Char
-- digitP = P.satisfy isDigit "digit"

-- smalllargedigitP :: P.Parser Char Char
-- smalllargedigitP = P.satisfy (\c -> isUpper c || isLower c || isDigit c || c=='_') "alphabetic character or digit or underscore"


-- mapReserved :: (Token, String) -> (Token, String)
-- mapReserved (TVarid, "case")     = (TCase, "case")
-- mapReserved (TVarid, "class")    = (TClass, "class")
-- mapReserved (TVarid, "data")     = (TData, "data")
-- mapReserved (TVarid, "default")  = (TDefault, "default")
-- mapReserved (TVarid, "deriving") = (TDeriving, "deriving")
-- mapReserved (TVarid, "do")       = (TDo, "do")
-- mapReserved (TVarid, "else")     = (TElse, "else")
-- mapReserved (TVarid, "if")       = (TIf, "if")
-- mapReserved (TVarid, "import")   = (TImport, "import")
-- mapReserved (TVarid, "in")       = (TIn, "in")
-- mapReserved (TVarid, "infix")    = (TInfix, "infix")
-- mapReserved (TVarid, "infixl")   = (TInfixl, "infixl")
-- mapReserved (TVarid, "infixr")   = (TInfixr, "infixr")
-- mapReserved (TVarid, "instance") = (TInstance, "instance")
-- mapReserved (TVarid, "let")      = (TLet, "let")
-- mapReserved (TVarid, "module")   = (TModule, "module")
-- mapReserved (TVarid, "newtype")  = (TNewtype, "newtype")
-- mapReserved (TVarid, "of")       = (TOf, "of")
-- mapReserved (TVarid, "then")     = (TThen, "then")
-- mapReserved (TVarid, "type")     = (TType, "type")
-- mapReserved (TVarid, "where")    = (TWhere, "where")
-- mapReserved (TVarid, "_")        = (TUnderscore, "_")
-- mapReserved x                    = x




--------------------------------------------------------------
---- ON TOKENS 
-- qvarTP :: P.TParser SToken
-- qvarTP = P.token TVarid <|> 
--         P.between (P.stoken (TSpecial, "(")) (P.stoken (TSpecial, ")")) 
--             (P.token TVarsym)

-- conTP :: P.TParser SToken
-- conTP = P.Parser $ \input -> case P.runParser (P.token TConid) input of
--                         Left x -> Left x
--                         Right r@(((t,s),pos), _) -> if '.' `elem` s 
--                                 then Left (ParseError (ParseUnexpected s "non-qualified constructor") pos)
--                                 else Right r

-- varTP :: P.TParser SToken
-- varTP = P.Parser $ \input -> case P.runParser (P.token TVarid) input of
--                         Left x -> Left x
--                         Right r@(((t,s),pos), _) -> if '.' `elem` s 
--                                 then Left (ParseError (ParseUnexpected s "non-qualified variable") pos)
--                                 else Right r