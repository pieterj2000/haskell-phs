module Parser
(
) where

import Error (Error)
import ExprDef (Module)


import qualified ParserCombs as P




parseFile :: String -> String -> HExpr