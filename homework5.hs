import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

let2intU :: Char -> Int
let2intU c = ord c - ord 'A'

int2letU :: Int -> Char
int2letU n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
	| isLower c = int2let ((let2int c + n) `mod` 26)
	| isUpper c = int2letU ((let2intU c + n) `mod` 26)
	| otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

p1 n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 + y^2 == z^2]
p2 n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]
p3 n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
p4 n = [(x,y,(x^2 + y^2)) | x <- [1..n], y <- [1..n]]

factors n = [x | x <- [1..n], n `mod` x == 0]

perfects1 n = [x | x <- [1..n], isPerfect x]
	where isPerfect num = sum(factors num) == num

perfects2 n = [x | x <- [1..n], isPerfect x]
	where isPerfect num = sum(init(factors num)) == num

perfects3 n = [isPerfect x | x <- [1..n]]
	where isPerfect num = sum(init(factors num)) == num

--perfects4 n = [x | x <- [1..n], isPerfect x]
--	where isPerfect num = init(factors num) == num

t = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
t1 = [z | z <- [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]]
t2 = concat [[[(x,y)] | x <- [1,2,3]] | y <- [4.5,6]]
--t3 = concat [(x,y) | y <- [4,5,6]] | x <- [1,2,3]
t4 = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

xs = 1 : [ x + 1 | x <- xs]

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions1 :: (Eq a) => a -> [a] -> [Int]
positions1 x xs = find x (zip xs [0..n])
	where n = length xs - 1

--positions2 :: (Eq a) => a -> [a] -> [Int]
--positions2 x xs = find x xs

--positions3 :: (Eq a) => a -> [a] -> [Int]
--positions3 x xs = find x (zipWith (+) xs [0..n])
--	where n = length xs - 1

--positions4 :: (Eq a) => a -> [a] -> [Int]
--positions4 x xs = find n (zip xs [0..x])
--	where n = length xs - 1

scalarproduct1 xs ys = sum [x * y | x <- xs, y <- ys]

scalarproduct2 xs ys = sum [x * y | (x,y) <- xs `zip` ys]

scalarproduct3 xs ys = product (zipWith (+) xs ys)

--scalarproduct4 xs ys = sum (product [ (x,y) | x <- xs, y <- ys])

riffle2 xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

divides x y = x `mod` y == 0

divisors x = [d | d <- [1..x], x `divides` d]

main :: IO()

main = do
	print "test"

	--print (p1 10)
	--print (p2 10)
	--print (p3 10)
	--print (p4 10)

	--print(perfects1 500)
	--print(perfects2 500)
	--print(perfects3 500)
	----print(perfects4 500)

	--print t
	--print t1
	--print t2
	----print t3 
	--print t4
	--print xs

	--print (positions1 0 [1,0,0,1,0,0])
	----print (positions2 0 [1,0,0,1,0,0])
	----print (positions3 0 [1,0,0,1,0,0])
	----print (positions4 0 [1,0,0,1,0,0])

	--print (scalarproduct1 [1,2,3] [4,5,6])
	--print (scalarproduct2 [1,2,3] [4,5,6])
	--print (scalarproduct3 [1,2,3] [4,5,6])
	----print (scalarproduct4 [1,2,3] [4,5,6])

	--print (encode 13 "Think like a Fundamentalist Code like a Hacker")
	print (riffle2 [1,2,3] [4,5,6])
