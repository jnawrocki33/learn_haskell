-- warm up functions this is a comment
doubleMe x = x + x
addSquares x y = x*x + y*y
guessNumber num = do
	putStrLn "Enter your guess:"
	guess <- getLine -- binds the input to guess
	if read guess < num
		then do
			putStrLn "Too Low!"
			guessNumber num
		else if read guess > num
			then do
				putStrLn "Too High!"
				guessNumber num
			else do
				putStrLn "You Win!"

factorial n
	|n == 0 = 1
	|n > 0 = n * factorial (n-1)

fib n	
	|n==0 = 1
	|n ==1 = 1
	|n > 1 = fib (n-1) + fib(n-2)
	|otherwise = 0

sort [] = []
sort (x:xs) =
	sort [b | b <- xs, b < x]
	++ [x] ++
	sort [b | b <- xs, b >= x]	