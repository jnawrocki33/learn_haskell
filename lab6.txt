-- Jason Nawrocki
-- Haskell assignment. Lab 6

-- This function checks if a palindrome is a string
-- It uses a recursive reverse function, and checks if the original string = reverse string
palindrome string = check string
	where
		check s = s == reverse(s)
		reverse (x:xs) = reverse(xs) ++ [x]
		reverse [] = []

-- This function checks if an array is a magic box (sum rows = sum columns = sum diagonals)
-- It builds an array containing the sums of all the rows and columns and diagonals, then checks
--    if each element in the array is equal
-- getRows works by simply summing each element in the 2x2 array
-- getCols works by first flipping the 2x2 array (so rows = cols), then summing rows in the flipped list
-- flipList builds a new 2x2 array where rows = cols
-- getDiag adds up the two diagonals

magicBox array = check array
	where
		check a = checkList (getRowSums a ++ getColSums a ++ getDiagSums a)
		getRowSums a = [sum (a!!i)|i <- [0..length(a)-1]]
		getColSums a = [sum ((flipList a)!!i)|i <- [0..length(a)-1]]
		flipList a = [[a!!i!!j|i <- [0..length(a)-1]]| j <- [0..length(a)-1]]
		getDiagSums a = [sum[(a!!i!!i)|i <- [0..length(a)-1]]] ++ [sum[(a!!i!!(length(a)-i-1))|i <- [0..length(a)-1]]]
		sum [] = 0
		sum(x:xs) = x + sum xs
		checkList b = and[b!!i == b!!(i+1)|i <- [0..length(b)-2]]


-- This is the quick sort function, modified so it sorts around the middle element
-- It recursively sorts, the elements less than the middle element,
--   then adds to this all elements equal to the middle element,
--   then adds to this the sorted elements larger than the middle element.

sort [] = []
sort (x) =
	sort [b | b <- x, b < (x!!(length(x) `quot` 2))]
	++ [b | b <- x, b == (x!!(length(x) `quot` 2))] ++
	sort [b | b <- x, b > (x!!(length(x) `quot` 2))]

-- returns solutions to the n-queens problem. naive approach, explained blow.
queens n = solve n
	where
		solve 0 = [ [] ]
		solve k = [h:partial | h <- [0..(n-1)], partial <- solve(k-1), safe h partial]
		safe h partial = and[not (checks h partial i) | i <- [0.. (length partial)-1] ]
		checks h partial i = h == partial!!i || abs(h-partial!!i) == i+1

-- In this ordering of evaluation, far more permuations are being calculated.
-- This is because partial <- solve(k-1) is executed before h
-- Therefore, even when the program is building a solution which does not work, it builds all possible
--    solutions, beecause it hasn't yet computed h to realize that all the following solutions check h.
-- For example, the first computation is building solutions with h = 0. The program starts building 
--   solutions with h = 0 in the first column, and then h = 0 in the second column, and isn't
--   able to detect the checking yet because it wants to compute all the possible solutions immedietaly.
-- So it checks all of [0,0,0,...0], [0,0,0,...1], [0,0,0,...2] etc
-- Whereas with the other ordering, once it realizes that [0,0,..] doesn't work, it moves on to [0,1,..]
-- This saves checking lots of possible solutions.