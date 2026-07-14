module Data.Map (
    Map (),
    fromList,
    empty,
    toList,
    elems,
    keys
) where


--TODO efficientere implementatie :)

newtype Map k a = Map { mappings :: [(k,a)] } deriving (Show) --TODO Show instance

empty :: Map k a
empty = fromList []

fromList :: [(k,a)] -> Map k a
fromList = Map

toList :: Map k a -> [(k, a)]
toList = mappings

elems :: Map a b -> [b]
elems = map snd . toList

keys :: Map b1 b2 -> [b1]
keys = map fst . toList