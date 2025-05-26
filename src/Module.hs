module Module (
    parseModule
) where

import Data.Functor (($>))


import qualified ParserCombs as P
import ExprDef (Module(..), Token (..), Pos (..))
import Error ( Error (..), ParseError (ParseModuleFileName) )
import Lexer (tokenize)
import Control.Applicative (Alternative(many, (<|>)))
import Control.Monad ((<=<), (>=>))
import Data.Char (isSpace)

-- TODO verplaatsen naar iets van utils?
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

-- TODO verplaatsen naar iets van utils?
-- | strips prefix from list if it has it, otherwise ignores it
stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix [] xs = xs
stripPrefix (x:xs) (y:ys)
    | x == y    = stripPrefix xs ys
    | otherwise = y:ys
stripPrefix _ [] = []

-- TODO verplaatsen naar iets van utils?
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute a b (x:xs)
    | x == a    = b : substitute a b xs
    | otherwise = x : substitute a b xs

-- Filename, contents
parseModule :: String -> String -> Either Error Module
parseModule filename content =
        let tokens = tokenize content
            tokens' = filter (\((t,_),_) -> t /= TWhiteSpace && t /= TNewLine ) <$> tokens
            output = tokens' >>= P.parseResult moduleP
            -- TODO nog door layout voeren
        in case output of
            Left e -> Left $ e filename
            Right x ->
                let mname = name x
                    supposedname = filter (not . isSpace) . substitute '/' '.' . stripPrefix "./" $ filename
                in if mname == supposedname
                    then Right x
                    else Left $ ParseError (ParseModuleFileName mname supposedname) (Pos 0 0) filename


-- TODO checken dat hele file geparsed is. Dus of niet parseResult gebruiken maar parse, en dan kijken dat rest==[], 
-- of char EOF aan moduleP toevoegen oid, is denk ik makkelijker

--TODO nog goed doen
moduleP :: P.TParser Module
moduleP = Module <$> (fst <$> moduleHeaderP) <*> pure []

data Export
    = Export

moduleHeaderP :: P.TParser (String, [Export])
moduleHeaderP = (,)
    <$> (P.token TModule *> P.tokens TConid )
    <*> P.between (P.stoken (TSpecial, "(")) (P.stoken (TSpecial, ")")) (many exportP)
    <* P.token TWhere
    -- <|> pure ("Main", []) -- TODO default Main moet 'main' exporteren

exportP = undefined