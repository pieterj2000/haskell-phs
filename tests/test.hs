

if' :: Bool -> (a -> (a,b,[c],d)) -> a -> [(a,b)]
if' p t f = case p of { True -> t; False -> f }

fact :: Integer -> [Integer]
fact n = if' (n == 0) 1 (n * fact (n-1))

data Bool = True | False

predicate :: Bool
predicate = False

predicate :: Bool -> Integer
toint p = p 1 0

main :: Integer
main = fact 10

undefined :: a
undefined = undefined
