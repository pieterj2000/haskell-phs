module Parser.Fixity (
    solveFixity
) where

import Error
import ExprDef

solveFixity :: [(String, VarInfo)] -> HExpr -> Either Error HExpr
solveFixity info (HInfixExpr xs) = undefined
solveFixity _ x = Right x


parse :: [HExpr] -> HExpr
parse ( HInfixOp "-" :b:rest) = undefined