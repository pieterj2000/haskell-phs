module Lexer.Layout
(

) where

import ExprDef (Token(..), Pos(..), SToken)



rule1 :: [LToken] -> [LToken]
rule1 [] = []
rule1 [x] = [x]
rule1 (t1@(LToken (x,_)) : t2@(LToken (y,p)) : xs)
    | isKeyword x && y /= TBracketOpen   = t1 : Layout p : rule1 (t2:xs)
    | otherwise                         = t1 : rule1 (t2:xs)
rule1 (x:xs) = x : rule1 xs

rule2 :: [LToken] -> [LToken]
rule2 [] = []
rule2 [x] = [x]
rule2 (t1@(LToken (TWhiteSpace,_)) : t2 : xs) = t1 : rule2 (t2:xs)
rule2 (t1@(LToken (TNewLine,_)) : t2 : xs) = t1 : rule2 (t2:xs)
rule2 (t1@(LToken (TModule,_)) : t2 : xs) = t1:t2:xs
rule2 (t1@(LToken (TBracketOpen,_)) : t2 : xs) = t1:t2:xs
rule2 (t1@(LToken (_,p)) : t2 : xs) = Layout p:t1:t2:xs
rule2 _ = error "Parsing layout, rule 2, Token list does not start with token. This should not happen."

splitOnLineBreak :: [LToken] -> [[LToken]]
splitOnLineBreak [] = []
splitOnLineBreak xs = if null line then [] else line : splitOnLineBreak rest
    where
        isnewl (LToken (TNewLine,_)) = True
        isnewl _                    = False
        spul = dropWhile isnewl xs
        (line, rest) = break isnewl spul

rule3 :: [LToken] -> [LToken]
rule3 = concatMap rule3line . splitOnLineBreak

rule3line :: [LToken] -> [LToken]
rule3line tokens = if null stuff
        then white
        else case head stuff of
            Layout _                -> tokens
            Line _                  -> error "Parsing layout, rule 3, line already starts with Line token. This should not happen."
            LToken (x, p) -> white ++ [Line p] ++ stuff
    where
        iswhite (LToken (TWhiteSpace, _)) = True
        iswhite _                        = False
        (white, stuff) = break iswhite tokens

addContexts :: [SToken] -> [LToken]
addContexts = rule3 . rule2 . rule1 . map LToken


data LToken = LToken SToken | Layout Pos | Line Pos

handleLayout :: [LToken] -> [SToken]
handleLayout tokens = hL tokens []


isKeyword :: Token -> Bool
isKeyword = undefined


hL :: [LToken] -> [Pos] -> [SToken]
hL ((Line p@(Pos col row)) : ts) (m@(Pos mcol _):ms)
    | mcol == (col+1)   = (TSemicolon, p) : hL ts (m:ms)
    | (col+1) < mcol    = (TBracketClose, p) : hL (Line p : ts) ms
    | (col+1) > mcol    = hL ts (m:ms)
hL ((Line p) : ts) []
                    = hL ts []
hL ((Layout p@(Pos col row)) : ts) []
    | (col+1) == 0      = undefined --TODO errormessage. Dit zou sowiseso nooit moeten kunnen, dus dit is een error in het programma, niet van de inputfile
    | (col+1) > 0       = (TBracketOpen, p) : hL ts [Pos (col+1) row]
hL ((Layout p@(Pos col row)) : ts) (m@(Pos mcol _):ms)
    | (col+1) > mcol    = (TBracketOpen, p) : hL ts ((Pos (col+1) row):m:ms)
    | (col+1) <= mcol   = (TBracketOpen, p) : (TBracketClose, p) : hL (Line p : ts) (m:ms)
hL ((LToken (TBracketClose,p)) : ts) ((Pos 0 0):ms)
                = (TBracketClose,p) : hL ts ms
hL ((LToken (TBracketClose,p)) : ts) ms
                = undefined -- TODO errormessage. Dit is wél een error in het inputfile. Namelijk: een expliciete } zonder expliciete {
hL ((LToken (TBracketOpen,p)) : ts) ms
                = (TBracketOpen,p) : hL ts ((Pos 0 0):ms)
hL tokens@(LToken t : ts) (m:ms)
    | m /= (Pos 0 0) && undefined {-TODO: hier is de parse-error condition. Dit moeten we dus nog even verder uitwerken.... -} = (TBracketClose, snd t) : hL tokens ms
    | otherwise = t : hL ts (m:ms)
hL (LToken t :ts) ms
                = t : hL ts ms
hL [] []
                = []
hL [] (m:ms)
    | m /= (Pos 0 0)    = undefined -- TODO errormessage. Dit is wél een error in het inputfile. Namelijk: een expliciete } zonder expliciete {
    | otherwise                 = (TBracketClose, m) : hL [] ms
