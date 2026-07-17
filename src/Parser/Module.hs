{-# LANGUAGE RankNTypes #-}
module Parser.Module (
    -- parseModule,
    --parseFile
    parseFile) where


import Defs.ExprDefs (Module (Module), VarStore, VarInfo (..), FixityType (..), Decl (..), CExpr)
import Error (Error (..))
import Defs.Haskell (HDecl (..))
import Parser.Parser (parseModuleBody)
import Parser.Lexer (tokenize)
import Data.Maybe (mapMaybe)
import Parser.Fixity (solveFixity)
import Desugar (desugarToCore)
import qualified Data.Map as M
import Control.Monad.Trans.Either (EitherT (runEitherT), lift, liftEither)



varinfoDefault :: VarStore
varinfoDefault = let (-->) = (,) in
    [ "(-)" --> VarInfo InfixL 6 Nothing
    , "(+)" --> VarInfo InfixL 6 Nothing
    , "(*)" --> VarInfo InfixL 7 Nothing
    ]


-- TODO ergens een check dat er maar één definitie, type, en infix declaration per functie is....

--TODO naar andere file (misschien iets van een Parser.Postprocess, of misscien naar Parser, en dan alles wat in de huidige Parser staat naar Parser.Parser of zo?)
-- | adds all fixity declaration to the variable store, so that sequences of infix operations 
--   can be resolved. Note that this does not add definitions yet to the varstore
--TODO ook daadwerkelijke infix opslaan zodra infix declarations ook echt werken :)
--TODO alle types van HDecl doen
registerFixity :: VarStore -> [HDecl] -> VarStore
registerFixity vars decls = 
    let fundefs = mapMaybe (\x -> case x of { (HFuncDef naam _) -> Just naam; _ -> Nothing }) decls
    in foldr (\naam store -> (naam, VarInfo InfixL 9 Nothing) : store) vars fundefs




-- TODO hier een fatsoenlijke monad van maken voor errors misschien
-- TODO die Varstore moet eigenlijk in Module verwerkt worden samen met de CExpressions
parseFile :: String -> (forall a. (Show a) => String -> a -> IO ()) -> IO (Either Error (VarStore, Module CExpr))
parseFile filenaam printv = runEitherT $ do
    input <- lift $ readFile filenaam
    -- parse 'header', i.e. vind welke imports hij heeft
    -- parse die imports
    -- TODO: overweeg om éérst een graaf te bouwen van files, en dan pas iedere geheel te parsen en compilen, niet hier recursief ze allemaal doen, want dan blijft alles in geheugen toch?
    -- dán pas deze zelf parsen (of misschien wel al parsen maar nog niet desugaren en derglijke)
    -- en alles desugeren en dergelijke
    lift $ printv "input" input
    lift $ printv "tokens" $ tokenize input
    ast <- liftEither $ parseModuleBody filenaam input
    lift $ printv "ast" ast
    let varinfo = varinfoDefault -- TODO 
        varinfof = registerFixity varinfo ast
    -- TODO fixity solven moet naar desugar!!!!
    fast <- liftEither $ solveFixity varinfof ast
    let core = concatMap desugarToCore fast
        -- TODO deze map vullen is rommelig zo, beter maken
        m = M.fromList [ (naam, expr) | (Decl naam expr) <- core]
    lift $ printv "ast na fixity" fast
    lift $ mapM_ (printv "core") core
    pure (varinfof, Module "moduleName" filenaam [] [] m)




-- parseFile :: String -> String -> Either Error (Module HDecl)
-- parseFile filename input = fst <$> runParserLex (moduleP filename) filename input


-- moduleP :: String -> P.Parser SString Error (Module HDecl)
-- moduleP filename = 
--     (Module 
--         "TODO modulename" 
--         filename
--         [] -- TODO moduleImports
--         [] -- TODO moduleExports
--         -- . M.fromList . map (\x@(HDecl s _ _ _) -> (s,x)) . pure) <$> declaration






























-- TODO ergens op een goeie plek zetten en goed doen 
-- parseFile :: String -> String -> Either Error Module
-- parseFile = parseModule




-- TODO verplaatsen naar iets van utils?
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

-- TODO verplaatsen naar iets van utils?
-- | strips prefix from list if it has it, otherwise ignores it
-- first argument is the prefix, second is the string 
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

-- TODO verplaatsen naar iets van utils, en kijken of dit niet efficienter kan (gebruiken nu om .hs van namen af te strippen)
stripPostfix :: Eq a => [a] -> [a] -> [a]
stripPostfix post string = reverse $ stripPrefix (reverse post) (reverse string)



-- -- Filename, contents
-- parseModule :: String -> String -> Either Error Module
-- parseModule filename content =
--         let tokens = tokenize content
--             output = tokens >>= P.parseResult moduleP
--             -- TODO nog door layout voeren
--         in case output of
--             Left e -> Left $ e filename
--             Right x@(name, exports, imports, defs) ->
--                 let supposedname = stripPostfix ".hs" . filter (not . isSpace) . substitute '/' '.' . stripPrefix "./" $ filename
--                     -- TODO misschien die strippostfix vervangen met takeWhile (/='.'), dan heb je ook meteen dat .lhs werkt
--                     -- TODO literate haskell laten werken
--                 in if name == supposedname
--                     then Right $ Module name [] []
--                     else Left $ ParseError (ParseModuleFileName name supposedname) (Pos 1 1) filename --TODO errors ook met getLoc doen 


-- TODO checken dat hele file geparsed is. Dus of niet parseResult gebruiken maar parse, en dan kijken dat rest==[], 
-- of char EOF aan moduleP toevoegen oid, is denk ik makkelijker

--TODO nog goed doen 
-- moduleP :: P.TParser (String, [ExportParse], [ImportParse], [TopLevelDef])
-- moduleP = (\(a,b) c d -> (a,b,c,d)) <$> moduleHeaderP <*> pure [] <*> pure []

data TopLevelDef

-- data ExportParse -- TODO op een manier Pos nog mee aan vast maken, zodat bij het checken goeie errors gegeven kunnen worden
--     = ExportVar String Pos
--     | ExportConsall String Pos
--     | ExportCons String Pos [(String, Pos)]
--     | ExportConsnone String Pos

--  -- TODO hs-boot

-- data ImportParse -- naam, dan qulified, nothing is nee, of Just Q
--     = ImportParse String Pos (Maybe String) ImportThings
-- data ImportThings
--     = ImportAll
--     | ImportListed [ImportThing]
--     | ImportHiding [ImportThing]
-- data ImportThing
--     = ImportVar String Pos
--     | ImportConsAll String Pos
--     | ImportCons String [(String, Pos)] Pos
--     | ImportConsnone String Pos

-- moduleHeaderP :: P.TParser (String, [ExportParse])
-- moduleHeaderP = (,)
--     <$> (P.token TModule *> P.tokens TConid )
--     <*> P.between (P.stoken (TSpecial, "(")) (P.stoken (TSpecial, ")")) exportsP
--     <* P.token TWhere
--     <|> pure ("Main", [ExportVar "main" (Pos 1 1)]) -- TODO default Main moet 'main' exporteren


-- exportsP :: P.Parser SToken [ExportParse]
-- exportsP = ((++) <$> many (exportP <* P.stoken (TSpecial, ",")) <*> ( (:[]) <$> exportP) <|> pure [])
--             <|> pure []

-- exportP :: P.TParser ExportParse
-- exportP = (\((t,s),p) -> ExportVar s p) <$> P.withPos qvarTP
--         <|> (uncurry ExportConsall <$> P.withPos (P.tokens TConid)) <* P.stoken (TSpecial, "(") <* P.stoken (TSpecialOp, "..") <* P.stoken (TSpecial, ")")
--         <|> uncurry ExportCons <$> P.withPos (P.tokens TConid) <*> 
--             P.between (P.stoken (TSpecial, "(")) (P.stoken (TSpecial, ")")) 
--             (many $ P.withPos (snd <$> conTP <|> snd <$> varTP))
--         <|> (uncurry ExportConsnone <$> P.withPos (P.tokens TConid))