module Lexer.Layout
(

) where

import ExprDef (Token(..), Pos(..), SToken)



rule1 :: [LToken] -> [LToken]
rule1 [] = []
rule1 [x] = [x]
rule1 (t1@(LToken (x,_)) : t2@(LToken (y,Pos col _)) : xs)
    | isKeyword x && y /= BracketOpen   = t1 : Layout col : rule1 (t2:xs)
    | otherwise                         = t1 : rule1 (t2:xs)
rule1 (x:xs) = x : rule1 xs

rule2 :: [LToken] -> [LToken]
rule2 [] = []
rule2 [x] = [x]
rule2 (t1@(LToken (Module,_)) : t2@(LToken (y,Pos col _)) : xs) = t1:t2:xs
rule2 (t1@(LToken (BracketOpen,_)) : t2@(LToken (y,Pos col _)) : xs) = t1:t2:xs
rule2 (t1 : t2@(LToken (_,Pos col _)) : xs) = Layout col:t1:t2:xs
rule2 _ = error "Parsing layout, rule 2, Token list does not start with token. This should not happen."

splitOnLineBreak :: [LToken] -> [[LToken]]
splitOnLineBreak [] = []
splitOnLineBreak xs = if null line then [] else line : splitOnLineBreak rest
    where
        isnewl (LToken (NewLine,_)) = True
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
            LToken (x, Pos col row) -> white ++ [Line col] ++ stuff
    where
        iswhite (LToken (WhiteSpace, _)) = True
        iswhite _                        = False
        (white, stuff) = break iswhite tokens

addContexts :: [SToken] -> [LToken]
addContexts = rule3 . rule2 . rule1 . map LToken


data LToken = LToken SToken | Layout Int | Line Int
handleLayout :: [Token] -> [Token]
handleLayout tokens = handleLayoutdoe

isKeyword :: Token -> Bool
isKeyword = undefined

handleLayoutdoe :: [LToken] -> [Int] -> [Token]
handleLayoutdoe = undefined
