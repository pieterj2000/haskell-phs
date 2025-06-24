module Parser.Lexer.Layout
(
    -- handleLayout
) where

-- import ExprDef (Token(..), Pos(..), SToken, PToken)
-- import Error (Error (ParseError), ParseError (LayoutError))



-- rule1 :: [LToken] -> [LToken]
-- rule1 [] = []
-- rule1 [x] = [x]
-- rule1 (t1@(LToken (x,_)) : t2@(LToken (y,p)) : xs)
--     | isKeyword (fst x) && fst y /= TBracketOpen    = t1 : Layout p : rule1 (t2:xs)
--     | otherwise                                     = t1 : rule1 (t2:xs)
-- rule1 (x:xs) = x : rule1 xs

-- rule2 :: [LToken] -> [LToken]
-- rule2 [] = []
-- rule2 [x] = [x]
-- rule2 (t1@(LToken ((TWhiteSpace,_),_)) : t2 : xs) = t1 : rule2 (t2:xs)
-- rule2 (t1@(LToken ((TNewLine,_),_)) : t2 : xs) = t1 : rule2 (t2:xs)
-- rule2 (t1@(LToken ((TModule,_),_)) : t2 : xs) = t1:t2:xs
-- rule2 (t1@(LToken ((TBracketOpen,_),_)) : t2 : xs) = t1:t2:xs
-- rule2 (t1@(LToken (_,p)) : t2 : xs) = Layout p:t1:t2:xs
-- rule2 _ = error "Parsing layout, rule 2, Token list does not start with token. This should not happen."

-- splitOnLineBreak :: [LToken] -> [[LToken]]
-- splitOnLineBreak [] = []
-- splitOnLineBreak xs = if null line then [] else line : splitOnLineBreak rest
--     where
--         isnewl (LToken ((TNewLine,_),_)) = True
--         isnewl _                    = False
--         spul = dropWhile isnewl xs
--         (line, rest) = break isnewl spul

-- rule3 :: [LToken] -> [LToken]
-- rule3 = concatMap rule3line . splitOnLineBreak

-- rule3line :: [LToken] -> [LToken]
-- rule3line tokens = if null stuff
--         then white
--         else case head stuff of
--             Layout _                -> tokens
--             Line _                  -> error "Parsing layout, rule 3, line already starts with Line token. This should not happen."
--             LToken ((x,_), p) -> white ++ [Line p] ++ stuff
--     where
--         iswhite (LToken ((TWhiteSpace,_), _)) = True
--         iswhite _                        = False
--         (white, stuff) = break iswhite tokens

-- addContexts :: [PToken] -> [LToken]
-- addContexts = rule3 . rule2 . rule1 . map LToken


-- data LToken = LToken PToken | Layout Pos | Line Pos

-- handleLayout :: [PToken] -> Either (String -> Error) [PToken]
-- handleLayout tokens = hL (addContexts tokens) []


-- isKeyword :: Token -> Bool
-- isKeyword = undefined


-- hL :: [LToken] -> [Pos] -> Either (String -> Error) [PToken]
-- hL ((Line p@(Pos line col)) : ts) (m@(Pos _ mcol):ms)
--     | mcol == col   = ( ((TSemicolon,";"), p) : ) <$> hL ts (m:ms)
--     | col < mcol    = ( ((TBracketClose,"}"), p) : ) <$> hL (Line p : ts) ms
--     | col > mcol    = hL ts (m:ms)
-- hL ((Line p) : ts) []
--                     = hL ts []
-- hL ((Layout p@(Pos line col)) : ts) []
--     | col == 0      = error $ "Got token on column 0 (" <> show p <> "). This should not be able to happen" 
--     | col > 0       = ( ((TBracketOpen, "{"), p) : ) <$> hL ts [Pos line col]
-- hL ((Layout p@(Pos line col)) : ts) (m@(Pos _ mcol):ms)
--     | col > mcol    = ( ((TBracketOpen, "{"), p) : ) <$> hL ts ((Pos line col):m:ms)
--     | col <= mcol   = (\v -> ((TBracketOpen, "{"), p) : ((TBracketClose,"}"), p) : v) <$> hL (Line p : ts) (m:ms)
-- hL ((LToken ((TBracketClose,_),p)) : ts) ((Pos 0 0):ms)
--                 = ( ((TBracketClose,"}"),p) : ) <$> hL ts ms
-- hL ((LToken ((TBracketClose,_),p)) : ts) ms
--                 = Left $ ParseError (LayoutError "Got an explicit } matching to an implicit {") p 
-- hL ((LToken ((TBracketOpen,_),p)) : ts) ms
--                 = ( ((TBracketOpen, "{"),p) : ) <$> hL ts ((Pos 0 0):ms)
-- hL tokens@(LToken t@((tt,_),p) : ts) (m:ms)
--     | m /= (Pos 0 0) && undefined {-TODO: hier is de parse-error condition. Dit moeten we dus nog even verder uitwerken.... -} = ( ((TBracketClose,"}"),p) : ) <$> hL tokens ms
--     | otherwise = (t :) <$> hL ts (m:ms)
-- hL (LToken t :ts) ms
--                 = (t :) <$> hL ts ms
-- hL [] []
--                 = Right []
-- hL [] (m:ms)
--     | m /= (Pos 0 0)    = undefined -- TODO errormessage. Dit is wél een error in het inputfile. Namelijk: een expliciete } zonder expliciete {
--     | otherwise                 = ( ((TBracketClose,"}"), m) : ) <$> hL [] ms

-- hL ((Line (Pos _ _)):_) ((Pos _ _):_) = error ""
-- hL ((Layout (Pos _ _)):_) [] = error  ""
-- hL ((Layout (Pos _ _)):_) ((Pos _ _):_) = error "" 