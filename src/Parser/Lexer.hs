module Parser.Lexer (
    tokenize,
    -- withpos --todo deze eruit
-- , qvarTP
-- , conTP
-- , varTP
--  runParserLex,
--  SString) where
    Token(..),
    tokenize',
    tokenize''
) where

import Defs.ExprDefs

import Data.Char (isAlphaNum, isUpper, isLower, isDigit, digitToInt, ord, chr, isOctDigit, isHexDigit, isAsciiUpper)
import Control.Applicative (many, Alternative ((<|>), some), optional)
import Data.Functor (($>))
import Utils
import Error (Error, ParseError (..))
import qualified ParserCombs as P
import Data.Maybe (mapMaybe)
import Debug.Trace (trace, traceShowId, traceShow)



data LexerStateFase = InComment Int String | Niets -- | InString String | InStringGap
data LayoutFase = InsertNiets | InsertContext | InsertIndent -- Context is  {n}, indent is <n>
    deriving Eq
-- | LexerState fase line col input layoutfase layoutcontexts
data LexerState i = LexerState LexerStateFase Int Int [i] LayoutFase


-- TODO waarom is de rest niet allemaal [i]?
data Token i
    = Tsymbols [i]
    | Tinteger Integer
    | Tspecialsymb i
    | Tvarid String
    | Tconsid String
    | Treserved String
    | TChar Char
    | TString String
    deriving (Show, Eq)

data LToken = Token (Token Char) | Indent Int | Context Int -- Context is  {n}, indent is <n>
    deriving (Eq, Show)



insertInitialBracket :: [LToken] -> Bool
insertInitialBracket spul =
    let leeg = null spul
        isbracket = head spul == Token (Tspecialsymb '{') 
        ismodule = head spul == Token (Treserved "module")
    in leeg || leeg || (not isbracket && not ismodule)

-- doeLayout :: [LToken] -> [Token Char]
-- doeLayout spul = traceShow spul $ mapMaybe (\t -> case t of
--                     Token t -> Just t
--                     _ -> Nothing) spul  


doeLayout :: [LToken] -> Either Error [Token Char]
doeLayout spul = go spul []
    where
        a <: b = (a :) <$> b
        infixr <:
        go :: [LToken] -> [Int] -> Either Error [Token Char]
        go (Indent n : ts) (m : ms)     | m == n                = (Tspecialsymb ';') <: go ts (m : ms)
        go (Indent n : ts) (m : ms)     | n < m                 = (Tspecialsymb '}') <: go (Indent n : ts) ms
        go (Indent n : ts) ms                                   = go ts ms
        go (Context n : ts) (m : ms)    | n > m                 = (Tspecialsymb '{') <: go ts (n : m : ms)
        go (Context n : ts) []          | n > 0                 = (Tspecialsymb '{') <: go ts [n]
        go (Context n : ts) ms                                  = (Tspecialsymb '{') <: (Tspecialsymb '}') <: go (Indent n : ts) ms
        go (Token (Tspecialsymb '}') : ts) (0 : ms)             = (Tspecialsymb '}') <: go ts ms
        go (Token (Tspecialsymb '}') : ts) ms                   = Left $ layoutError
        go (Token (Tspecialsymb '{') : ts) ms                   = (Tspecialsymb '{') <: go ts (0 : ms)
        go (t : ts) (m : ms)            | m /= 0 && parseerror  = (Tspecialsymb '}') <: go (t : ts) ms
                    where parseerror = False -- TODO PARSE-ERROR LAYOUT
        go (Token t : ts) ms                                   = t <: go ts ms
        go [] []                                                = Right []
        go [] (m : ms)                  | m /= 0                = (Tspecialsymb '}') <: go [] ms
        go [] (m : ms)                                          = Left $ layoutError



tokenize :: [Char] -> [Token Char]
--tokenize = undefined


tokenize'' :: String -> Either Error [Token Char]
tokenize'' spul = do 
        tokens <- tokenize' (LexerState Niets 1 1 spul InsertNiets)
        if insertInitialBracket tokens
            then do
                tokens' <- tokenize' (LexerState Niets 1 1 spul InsertContext)
                doeLayout tokens'
            else doeLayout tokens 


tokenize' :: LexerState Char -> Either Error [LToken]


tokenize' (LexerState Niets col line [] InsertContext) = Right [Context 0] 
tokenize' (LexerState Niets col line [] _) = Right []


-- Newline
tokenize' (LexerState Niets col line spul layout) | isnewline = tokenize' $ LexerState Niets 1 (line + 1) rest layout'
    where 
        (isnewline, rest) = isNewLine spul
        layout' = if layout == InsertContext then InsertContext else InsertIndent



-- Alles als we in comment zitten
tokenize' (LexerState (InComment n comment) col line spul layouts) | take 2 spul == "{-" = tokenize' (LexerState (InComment (n+1) $ "-}" ++ comment) col line (drop 2 spul) layouts)
tokenize' (LexerState (InComment n comment) col line spul layouts) | take 2 spul == "-}" = if n > 1 
                                    then tokenize' (LexerState (InComment (n-1) $ "{-" ++ comment) col line (drop 2 spul) layouts)
                                    else
                                        let (col', line') = calcPos col line $ reverse comment
                                        in tokenize' (LexerState Niets col' line' (drop 2 spul) layouts)
                                            -- TODO willen we hier error gooien? Kunnen ook gewoon de sluitende dingen impliciet toevoegen
tokenize' (LexerState (InComment n comment) col line [] layouts) = Left $ unexpectedError "closing comment \"-}\"" "end of file" 
tokenize' (LexerState (InComment n comment) col line (x:xs) layouts) = tokenize' (LexerState (InComment n $ x : comment) col line xs layouts)

-- Beginnen met comment als we er niet in zitten
tokenize' (LexerState Niets col line spul layoutstate) | start && nietlang = tokenize' (LexerState (InComment 1 "-}") col line (drop 2 spul) layoutstate)
            where
                start = take 2 spul == "{-"
                nietlang = (drop 2 $ take 3 spul) /= "#"


-- Whitespace
tokenize' (LexerState Niets col line spul layouts) | isWhiteCharNoNewLine (head spul) = 
            let (white, rest) = span isWhiteCharNoNewLine spul
                (col', line') = calcPos col line white
            in tokenize' $ LexerState Niets col' line' rest layouts

-- Comment single line -- TODO hier layout newline voor gebruiken? Nee denk dat dit prima is, want we snijden alles af tot de nieuwline (zie nextLine)
-- we herbruiken hier col en line zonder nieuw te berekenen, aangezien de eerstvolgende token gegarandeerd een newline is die hem toch weer reset naar line+1 en 1
tokenize' (LexerState Niets col line spul@(s1:s2:rest) layoutstate) | s1 == '-' && s2 == '-' && restgeencomment = tokenize' $ LexerState Niets col line (nextLine rest') layoutstate
    where
        rest' = dropWhile (=='-') rest
        restgeencomment = null rest' || (not . isSymbol $ head rest')






-- Alles als we in een string zitten
--tokenize' (LexerState (InString _) [] layouts) = Left $ unexpectedError "end of string (\")" "end of file"-- TODO beter error
--tokenize' (LexerState (InString lit) ('"':rest) layouts) = (TString (reverse lit) :) <$> tokenize' (LexerState Niets rest layouts)
--tokenize' (LexerState (InString lit) ('"':rest) layouts) = (TString (reverse lit) :) <$> tokenize' (LexerState Niets rest layouts)


-- TODODOODOD NU HIER DOEN: HIERZO INDENTS EN LAYOUTS INVOEGEN AFHANKELIJK VAN LAYOUTSTATE EN DAN PATTERN MATCHEN OP ALLE 'ECHTE' LEXEMES HIERONDER, ALLE LAYOUT EN 'NEP' LEXEMES HIERBOVEN
--tokenize' (LexerState Niets col line spul InsertNiets) = 
tokenize' (LexerState Niets col line spul InsertIndent) = (Indent col :) <$> tokenize' (LexerState Niets col line spul InsertNiets)
tokenize' (LexerState Niets col line spul InsertContext) = do
                tokens <- tokenize' (LexerState Niets col line spul InsertNiets)
                if null tokens || head tokens /= Token (Tspecialsymb '{') 
                    then return $ Context col : tokens
                    else return tokens


-- chars parsen
tokenize' (LexerState Niets col line ('\'' : rest) layoutstate) = 
                    let (rest', result) = P.runParserMetConsumed charP rest
                    in do
                        (r, consumed) <- result
                        let (col', line') = calcPos col line $ consumed
                        toekomst <- tokenize' (LexerState Niets col' line' rest' layoutstate)
                        pure $ Token (TChar r) : toekomst


-- strings parsen
tokenize' (LexerState Niets col line ('"' : rest) layoutstate) = 
                    let (rest', result) = P.runParserMetConsumed stringP rest
                    in do
                        (lit, consumed) <- result
                        --(lit, rest') <- stringParse rest
                        let (col', line') = calcPos col line $ consumed
                        toekomst <- tokenize' (LexerState Niets col' line' rest' layoutstate)
                        pure $ Token (TString lit) : toekomst



-- Integers parsen  -- TODO floats
tokenize' (LexerState Niets col line spul layoutstate) | isDigit (head spul) = 
                    let (rest, result) = P.runParserMetConsumed intP spul
                    in do
                        (r, consumed) <- result
                        let (col', line') = calcPos col line $ consumed
                        --case result of
                        --Left e -> error $ "parsing int, dit zou niet moeten kunnen gebeuren, error: " ++ show e ++ " in string " ++ spul
                        toekomst <- tokenize' (LexerState Niets col' line' rest layoutstate)
                        --Right r -> (Tinteger r :) <$> tokenize' (LexerState Niets rest layouts)
                        pure $ Token (Tinteger r) : toekomst



-- Symbols (and special symbols)
tokenize' (LexerState Niets col line (s:rest) layouts)   | isSpecial s = (Token (Tspecialsymb s) :) <$> tokenize' (LexerState Niets (col+1) line rest layouts)
tokenize' (LexerState Niets col line spul@(s:_) layouts) | isSymbol s = 
                let (symbols, rest) = span isSymbol spul 
                    col' = col + (length symbols)
                in (Token (Tsymbols symbols) :) <$> tokenize' (LexerState Niets col' line rest layouts)

-- varid (en reservedid)
tokenize' (LexerState Niets col line spul@(s:rest) layoutstate) | isLower s || s=='_' = 
    let (varid, rest') = span (\c -> isAlphaNum c || (c=='_') || (c=='\'')) spul 
        col' = col + (length varid)
        layoutstate' = if varid `elem` ["let", "where", "do", "of"]
                then InsertContext else layoutstate
        toekomst = tokenize' $ LexerState Niets col' line rest' layoutstate'
    in if isReserved varid
        then (Token (Treserved varid) :) <$> toekomst -- TODO willen we dit niet gewoon als Tvarid doorgooien?
        else (Token (Tvarid varid) :) <$> toekomst

-- consid
tokenize' (LexerState Niets col line spul@(s:rest) layouts) | isUpper s = 
            let (consid, rest) = span (\c -> isAlphaNum c || (c=='_') || (c=='\'')) spul 
                col' = col + (length consid)
            in (Token (Tconsid consid) :) <$> tokenize' (LexerState Niets col' line rest layouts)




-- Snappen we niet
tokenize' (LexerState Niets col line spul layouts) = Left $ unexpectedError "iets wat we snappen" spul -- TODO hier betere error maken











calcPos :: Int -> Int -> String -> (Int, Int)
calcPos col line [] = (col, line)
calcPos col line ('\r':'\n':ss) = calcPos 1 (line + 1) ss
calcPos col line ('\r':ss) = calcPos 1 (line + 1) ss
calcPos col line ('\n':ss) = calcPos 1 (line + 1) ss
calcPos col line ('\f':ss) = calcPos 1 (line + 1) ss
calcPos col line ('\t':ss) = 
            let r = col `rem` 8
                extra = 8 - r
            in calcPos (col+extra+1) line ss
calcPos col line (c:ss) = calcPos (col+1) line ss






















tokenize [] = []

-- Integer
tokenize spul | isDigit (head spul) = let (digits, rest) = span isDigit spul in Tinteger (digitsToInt digits) : tokenize rest

-- Newline -- TODO layout fixen
tokenize spul | isnewline = Tspecialsymb ';' : tokenize rest
    where (isnewline, rest) = isNewLine spul

-- Whitespace
tokenize spul | isWhiteCharNoNewLine (head spul) = tokenize $ dropWhile isWhiteCharNoNewLine spul

-- Comment single line
tokenize spul@(s1:s2:rest) | s1 == '-' && s2 == '-' && restgeencomment = tokenize $ nextLine rest'
    where
        rest' = dropWhile (=='-') rest
        restgeencomment = null rest' || (not . isSymbol $ head rest')




-- Symbols (and special symbols)
tokenize (s:rest) | isSpecial s = Tspecialsymb s : tokenize rest
tokenize spul | isSymbol (head spul) = let (symbols, rest) = span isSymbol spul in Tsymbols symbols : tokenize rest

-- varid (en reservedid)
tokenize spul@(s:rest) | isLower s || s=='_' = 
    let (varid, rest) = span (\c -> isAlphaNum c || (c=='_') || (c=='\'')) spul 
    in if isReserved varid
        then Treserved varid : tokenize rest -- TODO willen we dit niet gewoon als Tvarid doorgooien?
        else Tvarid varid : tokenize rest

-- consid
tokenize spul@(s:rest) | isUpper s = let (consid, rest) = span (\c -> isAlphaNum c || (c=='_') || (c=='\'')) spul in Tconsid consid : tokenize rest


-- Snappen we niet
tokenize spul = [Tsymbols $ "error: resterende tokens die we niet snappen: '" ++ spul ++ "'"] -- TODO hier fatsoenlijk error maken? Of alleen error (dus crash)










-----------------------------------------------------------------

-- De openende " zit al in de lexer, de afsluitende zit hierin
charP :: P.Parser Char Error Char
charP = (   (P.satisfy isGraphicNietQuotesBackslash "character") 
        <|> (P.token ' ') 
        <|> (P.token '"') 
        <|> escapeP
        ) <* P.token '\''

-- -- input -> Either error (geparste string literal, rest of tokens)
-- stringParse :: [Char] -> Either Error (String, [Char])
-- stringParse spul = 
--         let (rest, result) = P.runParser stringP spul
--         in case result of 
--             Left e -> Left e
--             Right r -> Right (r, rest)

-- De openende " zit al in de lexer, de afsluitende zit hierin
stringP :: P.Parser Char Error [Char]
stringP = concat <$> many 
                (   (pure <$> P.satisfy isGraphicNietQuotesBackslash "character") 
                <|> stringGapP 
                <|> (pure <$> P.token ' ') 
                <|> (pure <$> P.token '\'') 
                <|> (pure <$> escapeP) 
                <|> (P.tokens "\\&" *> pure [])
                ) <* P.token '"'

stringGapP :: P.Parser Char Error [Char]
stringGapP = P.token '\\' *> many (P.satisfy isWhiteChar "whitespace") *> P.token '\\' *> pure []

escapeP :: P.Parser Char Error Char
escapeP = P.token '\\' *>
            (   P.getIf escapeChar "escape character"
            <|> (P.token '^' *> P.getIf escapeCharHoedje "escape character")
            <|> escapeAsciiAnders
            <|> (chr . fromInteger <$> decimalP)
            <|> (P.token 'o' *> (chr . fromInteger <$> octalP))
            <|> (P.token 'x' *> (chr . fromInteger <$> hexaP))
            )

escapeChar :: Char -> Maybe Char
escapeChar 'a' = Just $ chr 7
escapeChar 'b' = Just $ chr 8
escapeChar 'f' = Just $ chr 12
escapeChar 'n' = Just $ chr 10
escapeChar 'r' = Just $ chr 13
escapeChar 't' = Just $ chr 9
escapeChar 'v' = Just $ chr 11
escapeChar '\\' = Just $ '\\'
escapeChar '"' = Just $ '"'
escapeChar '\'' = Just $ '\''
escapeChar _ = Nothing

escapeCharHoedje :: Char -> Maybe Char
escapeCharHoedje c | isAsciiUpper c = Just . chr $ ord c - ord 'A' + 1
escapeCharHoedje '@' = Just $ chr 0
escapeCharHoedje '[' = Just $ chr 27
escapeCharHoedje '\\' = Just $ chr 28
escapeCharHoedje ']' = Just $ chr 29
escapeCharHoedje '^' = Just $ chr 30
escapeCharHoedje '_' = Just $ chr 31
escapeCharHoedje _ = Nothing

escapeAsciiAnders :: P.Parser Char Error Char
escapeAsciiAnders = 
        let num =   (P.tokens "NUL" *> pure 0)
                <|> (P.tokens "SOH" *> pure 1)
                <|> (P.tokens "STX" *> pure 2)
                <|> (P.tokens "ETX" *> pure 3)
                <|> (P.tokens "EOT" *> pure 4)
                <|> (P.tokens "ENQ" *> pure 5)
                <|> (P.tokens "ACK" *> pure 6)
                <|> (P.tokens "BEL" *> pure 7)
                <|> (P.tokens "BS" *> pure 8)
                <|> (P.tokens "HT" *> pure 9)
                <|> (P.tokens "LF" *> pure 10)
                <|> (P.tokens "VT" *> pure 11)
                <|> (P.tokens "FF" *> pure 12)
                <|> (P.tokens "CR" *> pure 13)
                <|> (P.tokens "SO" *> pure 14)
                <|> (P.tokens "SI" *> pure 15)
                <|> (P.tokens "DLE" *> pure 16)
                <|> (P.tokens "DC1" *> pure 17)
                <|> (P.tokens "DC2" *> pure 18)
                <|> (P.tokens "DC3" *> pure 19)
                <|> (P.tokens "DC4" *> pure 20)
                <|> (P.tokens "NAK" *> pure 21)
                <|> (P.tokens "SYN" *> pure 22)
                <|> (P.tokens "ETB" *> pure 23)
                <|> (P.tokens "CAN" *> pure 24)
                <|> (P.tokens "EM" *> pure 25)
                <|> (P.tokens "SUB" *> pure 26)
                <|> (P.tokens "ESC" *> pure 27)
                <|> (P.tokens "FS" *> pure 28)
                <|> (P.tokens "GS" *> pure 29)
                <|> (P.tokens "RS" *> pure 30)
                <|> (P.tokens "US" *> pure 31)
                <|> (P.tokens "SP" *> pure 32)
                <|> (P.tokens "DEL" *> pure 127)
        in chr <$> num
        




intP :: P.Parser Char Error Integer
intP =  ( (P.tokens "0o" <|> P.tokens "0O") *> octalP )
    <|> ( (P.tokens "0x" <|> P.tokens "0X") *> hexaP )
    <|> decimalP




decimalP, octalP, hexaP :: P.Parser Char Error Integer
decimalP = digitsToInt <$> some (P.satisfy isDigit "digit")
octalP = digitsToIntBase 8 <$> some (P.satisfy isOctDigit "digit")
hexaP = digitsToIntBase 16 <$> some (P.satisfy isHexDigit "digit")





------------------------------------------

digitsToIntBase :: Integer -> String -> Integer
digitsToIntBase base = foldl' (\acc el -> acc*base + toInteger el) 0 . map digitToInt

digitsToInt :: String -> Integer
digitsToInt = digitsToIntBase 10




isNewLine :: String -> (Bool, String)
isNewLine ('\r' : '\n' : rest) = (True, rest)
isNewLine ('\r' : rest) = (True, rest)
isNewLine ('\f' : rest) = (True, rest)
isNewLine ('\n' : rest) = (True, rest)
isNewLine rest = (False, rest)

-- | is inclusief de nieuwline
nextLine :: String -> String
nextLine [] = []
nextLine spul = case isNewLine spul of
    (True, rest) -> spul
    (False, _) -> nextLine $ tail spul


isWhiteChar :: Char -> Bool
isWhiteChar '\n' = True
isWhiteChar '\v' = True
isWhiteChar ' '  = True
isWhiteChar '\t' = True
isWhiteChar '\r' = True
isWhiteChar '\f' = True
isWhiteChar _ = False

isWhiteCharNoNewLine :: Char -> Bool
isWhiteCharNoNewLine '\v' = True
isWhiteCharNoNewLine ' ' = True
isWhiteCharNoNewLine '\t' = True
isWhiteCharNoNewLine _ = False




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


-- Graphic = small (= ascsmall, unismall, _), large (asclarge, unilarge), digit, symbol, special, ', "
isGraphicNietQuotesBackslash :: Char -> Bool
isGraphicNietQuotesBackslash '\'' = False
isGraphicNietQuotesBackslash '"' = False
isGraphicNietQuotesBackslash '\\' = False
isGraphicNietQuotesBackslash c = isAlphaNum c || isSpecial c || isDigit c || isSymbol c || c == '_'




isReserved :: String -> Bool
isReserved "case"       = True
isReserved "class"      = True
isReserved "data"       = True
isReserved "default"    = True
isReserved "deriving"   = True
isReserved "do"         = True
isReserved "else"       = True
isReserved "foreign"       = True
isReserved "if"         = True
isReserved "import"     = True
isReserved "in"         = True
isReserved "infix"      = True
isReserved "infixl"     = True
isReserved "infixr"     = True
isReserved "instance"   = True
isReserved "let"        = True
isReserved "module"     = True
isReserved "newtype"    = True
isReserved "of"         = True
isReserved "then"       = True
isReserved "type"       = True
isReserved "where"      = True
isReserved "_"          = True
isReserved _ = False







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

