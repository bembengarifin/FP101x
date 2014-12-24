type Bit = Int

--double t = x * 2

all1 p xs = and (map p xs)
--all2 p xs = map p (and xs)
all3 p = and . map p
all4 p = not . any (not . p)
--all5 p = map p . and
all6 p xs = foldl (&&) True (map p xs)
all7 p xs = foldr (&&) False (map p xs)
all8 p = foldr (&&) True . map p

dropWhile1 _ [] = []
dropWhile1 p (x : xs)
	| p x = dropWhile1 p xs
	| otherwise = x : xs

dropWhile2 _ [] = []
dropWhile2 p (x : xs)
	| p x = dropWhile2 p xs
	| otherwise = xs

dropWhile3 p = foldr (\ x acc -> if p x then acc else x : acc) []

dropWhile4 p = foldl add []
	where add [] x = if p x then [] else [x]
	      add acc x = x : acc

--any1 p = map p . or
any2 p = or . map p
any3 p xs = length (filter p xs) > 0
any4 p = not . null . dropWhile (not . p)
any5 p = null . filter p
any6 p xs = not (all (\ x -> not (p x)) xs)
any7 p xs = foldr (\ x acc -> (p x) || acc) False xs
any8 p xs = foldr (||) True (map p xs)

--map1 f = foldr (\ x xs -> xs || [f x]) []
map2 f = foldr (\ x xs -> f x ++ xs) []
map3 f = foldl (\ xs x -> f x : xs) []
map4 f = foldl (\ xs x -> xs ++ [f x]) []

filter1 p = foldl (\ xs x -> if p x then x : xs else xs) []
filter2 p = foldr (\ x xs -> if p x then x : xs else xs) []
filter3 p = foldr (\ x xs -> if p x then xs ++ [x] else xs) []
--filter4 p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []

dec2int1 = foldr (\ x y -> 10 * x + y) 0
dec2int2 = foldl (\ x y -> x + 10 * y) 0
dec2int3 = foldl (\ x y -> 10 * x + y) 0
dec2int4 = foldr (\ x y -> x + 10 * y) 0

curry1 f = \ x y -> f x y
curry2 f = \ x y -> f
curry3 f = \ x y -> f (x, y)
curry4 f = \ (x, y) -> f x y

uncurry1 f = \ (x, y) -> f x y
uncurry2 f = \ x y -> f (x, y)
uncurry3 f = \ (x, y) -> f
uncurry4 f = \ x y -> f

unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)



chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
--chop8 bits = take 8 bits : chop8 (drop 8 bits)


chop81 :: [Bit] -> [[Bit]]
chop81 [] = []
chop81 = unfold null (take 8) (drop 8)

--sumsqreven = compose [sum, map (^ 2), filter even]

--sumsqreven = compose [sum, map (^ 2), filter even]
--sumsqreven = compose [sum, map (^ 2), filter even]

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

main :: IO()

main = do
	print "test"

	print (uncurry1 mod (5,4))
	--print (uncurry2 mod (5,4))
	--print (uncurry3 mod (5,4))
	--print (uncurry4 mod (5,4))

{-
	print (curry fst 1 2)
	--print (curry1 fst 1 2)
	--print (curry4 fst 1 2)
	
	print (double 22)

	print (all1 odd [1,3,5])
	--print (all3 True)
	--print (all4 True)
	print (all6 odd [1,3,5])
	print (all7 odd [1,3,5])
	--print (all8 True)
	print (dropWhile1 even [2,4,5,6,7])
	print (dropWhile2 even [2,4,5,6,7])
	print (dropWhile3 even [2,4,5,6,7])
	print (dropWhile4 even [2,4,5,6,7])

	print (any even [1,2,3,5])
	print (any2 even [1,2,3,5])
	print (any3 even [1,2,3,5])
	print (any4 even [1,2,3,5])
	print (any5 even [1,2,3,5])
	print (any6 even [1,2,3,5])
	print (any7 even [1,2,3,5])
	print (any8 even [1,2,3,5])
	print (any8 even [1,3,5])

	print (map (*2) [1,2])
	--print (map2 (*2) [1,2])
	print (map3 (*2) [1,2])
	print (map4 (*2) [1,2])

--	print (filter even [1,2,3,4])
	print (filter1 even [1,2,3,4])
	print (filter2 even [1,2,3,4])
	print (filter3 even [1,2,3,4])

	print (dec2int1 [2,3,4,5])
	print (dec2int1 [])	
	print (dec2int1 [0,0,0,0])
	
	print (dec2int2 [2,3,4,5])
	print (dec2int2 [])
	print (dec2int2 [0,0,0,0])

	print (dec2int3 [2,3,4,5])
	print (dec2int3 [])
	print (dec2int3 [0,0,0,0])
	
	print (dec2int4 [2,3,4,5])	
	print (dec2int4 [])
	print (dec2int4 [0,0,0,0])
-}
