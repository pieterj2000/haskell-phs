
module Utils (
    mapLeft
, (.:)) 
where




-- TODO verplaatsen naar iets van utils?
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c


fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b 
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c
first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a,b,c)
second3 :: (b -> d) -> (a,b,c) -> (a,d,c)
second3 f (a,b,c) = (a,f b,c)
third3 :: (c -> d) -> (a,b,c) -> (a,b,d)
third3 f (a,b,c) = (a,b,f c)

