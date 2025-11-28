

if' p t f = p t f

fact n = if' (n == 0) 1 (n * fact (n-1))

data Bool = True | False

main = fact 10
