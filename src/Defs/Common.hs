module Defs.Common -- TODO exports
where



    

-- TODO kan dit niet gewoon een HExpr (of vergelijkbaars) zijn? Want het is effectief gewoon function application
data Type
    = TypeVar String
    | TypeConstr String
    | TypeApply Type Type
    | TypeArrow Type Type -- TODO eigenlijk moet dit gewoon een apply zijn, i.e. a -> b moet (->) a b zijn
                            -- dat doen we ook met tupels en lijsten en constructors en zo.
    deriving (Show)