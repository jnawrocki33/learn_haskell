factors n = [f|f <- [1..n], mod n f == 0]

factors2 n = [f|f <- [1..n], n `mod` f == 0]

primes = primeGen[2..]
	where
		primeGen(p:xs) = p:primeGen[q | q <- xs, mod q p /= 0]

recSum [] = 0
recSum(x:xs) = x + recSum xs

queens n = solve n
	where
		solve 0 = [ [] ]
		solve k = [h:partial | partial <- solve(k-1), h <- [0..(n-1)], safe h partial]
		safe h partial = and[not (checks h partial i) | i <- [0.. (length partial)-1] ]
		checks h partial i = h == partial!!i || abs(h-partial!!i) == i+1
