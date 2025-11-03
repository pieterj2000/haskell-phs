module Data.Map (
    Map (),
    fromList
) where


--TODO efficientere implementatie :)

newtype Map k a = Map { mappings :: [(k,a)] } deriving (Show) --TODO Show instance


fromList :: [(k,a)] -> Map k a
fromList = Map