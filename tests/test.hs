

if' p t f = p t f

fact n = if' (n == 0) 1 (n * fact (n-1))

data Bool = True | False

cons h t n c = c h t
head l = l 0 1
nil n c = n
inflist = cons 1 inflist

predicate = False
toint p = p 1 0
main = fact 2000

main''  = head inflist

const a b = a
cons2t a b = b
undefined = undefined
main2 = const 1 undefined
