module Parser
(
    parseFile
) where

import Module (parseModule)
import Error (Error)
import ExprDef (Module)


data HExpr
    = HExpr


data KHExpr
    = KHExpr
 

parseFile :: String -> String -> Either Error Module
parseFile = parseModule



