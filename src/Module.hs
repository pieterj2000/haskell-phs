module Module (
    parseModule
) where

import Data.Functor (($>))


import qualified ParserCombs as P
import ExprDef (Module(..), Token (..))
import Error ( Error )
import Lexer (tokenize)
import Control.Applicative (Alternative(many, (<|>)))


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

-- Filename, contents
parseModule :: String -> String -> Either [Error] Module
parseModule filename = mapLeft (map ($ filename)) . P.parseResult moduleP . tokenize
-- TODO checken dat hele file geparsed is. Dus of niet parseResult gebruiken maar parse, en dan kijken dat rest==[], 
-- of char EOF aan moduleP toevoegen oid, is denk ik makkelijker

moduleP :: P.TParser Module
moduleP = Module <$> (fst <$> moduleHeaderP) <*> pure []

data Export
    = Export

moduleHeaderP :: P.TParser (String, [Export])
moduleHeaderP = ( (,) <$> (P.token TModule *> P.modidP) <*> many exportP <* P.token TWhere ) -- <|> pure ("Main", []) -- TODO default Main moet 'main' exporteren

exportP = undefined
