double x = x + x

quadruple x = double (double x)

n = a `div` length xs
   where 
	a = 10
        xs = [1,2,3,4,5]

init1 xs = tail (reverse xs)
init2 xs = reverse (head (reverse xs))
init3 xs = reverse (tail xs)
init4 xs = take (length xs) xs
init5 xs = reverse (tail (reverse xs))
init6 xs = take (length xs - 1) (tail xs)
init7 xs = drop (length xs - 1) xs

sum [] = 0
sum (x:xs) = x + Main.sum xs

qsort1 [] = []
qsort1 (x:xs) = qsort1 larger ++ [x] ++ qsort1 smaller
	where 	smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort2 [] = []
qsort2 (x:xs) = reverse (qsort2 smaller ++ [x] ++ qsort2 larger)
	where 	smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort3 [] = []
qsort3 (x:xs) = reverse (qsort3 smaller) ++ [x] ++ reverse (qsort3 larger)
	where 	smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]
		
qsort4 [] = []
qsort4 xs = qsort4 larger ++ qsort4 smaller ++ [x]
	where 	x = minimum xs
		smaller = [a | a <- xs, a <= x]
		larger = [b | b <- xs, b > x]

qsort5 [] = []
qsort5 (x:xs) = qsort5 larger ++ [x] ++ qsort5 smaller
	where 	smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b > x || b == x]

qsort6 [] = []
qsort6 (x:xs) = qsort6 larger ++ [x] ++ qsort6 smaller
	where 	smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b > x]

qsort7 [] = []
qsort7 xs = x : qsort7 larger ++ qsort7 smaller
	where 	x = maximum xs
		smaller = [a | a <- xs, a < x]
		larger = [b | b <- xs, b >= x]

pair x y = (x, y)
double1 x = x * 2
palindrome xs = reverse xs == xs
e4 (x, y) = x
e6 x y = x * y
e7 (x, y) = (y, x)
e8 x y = (y, x)
e9 [x,y] = (x, True)
e10 (x,y) = [x,y]
e13 x y = x / y
e131 x y = x + y * y
twice f x = f (f x)

main :: IO()

main = do
	--print 5
	--print n
	--print (init1 [1,2,3,4,5])
	--print (init2 [1,2,3,4,5])
	--print (init3 [1,2,3,4,5])
	--print (init4 [1,2,3,4,5])
	--print (init5 [1,2,3,4,5])
	--print (init6 [1,2,3,4,5])
	--print (init7 [1,2,3,4,5])
	--print (Main.sum [1,2,1,2])
	print (qsort1 [1,2,3,4,5])
	print (qsort2 [1,2,3,4,5])
	print (qsort3 [1,2,3,4,5])
	--print (qsort4 [1,2,3,4,5])
	print (qsort5 [1,2,3,4,5])
	print (qsort6 [1,2,3,4,5])
	--print (qsort7 [1,2,3,4,5])



