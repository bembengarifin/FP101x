import Prelude hiding ((&&))

--halve1 xs = (take n xs, drop n xs)
--	where n = length xs / 2

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
	where n = length xs

--halve4 xs = splitAt (length xs `div` 2)

halve5 xs = (take n xs, drop (n+1) xs)
	where n = length xs `div` 2

halve6 xs = splitAt (div (length xs) 2) xs

--halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
	where n = length xs `div` 2

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3 (_ : xs)
	| null xs = []
	| otherwise = tail xs

safetail4 xs
	| null xs = []
	| otherwise = tail xs

--safetail5 xs = tail xs
--safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

safetail7 [x] = [x]
safetail7 (_ : xs) = xs

safetail8
	= \ xs ->
		case xs of
			[] -> []
			(_ : xs) -> xs

--a && b = if not (a) then not (b) else True
--a && b = if a then b else False
a && b = if b then a else False

remove n xs = take n xs ++ drop n xs

main :: IO()

main = do
	--print (halve2 [1,2,3,4])
	--print (halve3 [1,2,3,4])
	--print (halve5 [1,2,3,4])
	--print (halve6 [1,2,3,4])
	--print (halve8 [1,2,3,4])

	--print (safetail1 [] :: [Int])
	--print (safetail1 [1,2,3,4]) 
	--print (safetail2 [] :: [Int])
	--print (safetail2 [1,2,3,4]) 
	----print (safetail3 [] :: [Int])
	----print (safetail3 [1,2,3,4]) 
	--print (safetail4 [] :: [Int])
	--print (safetail4 [1,2,3,4]) 
	--print (safetail6 [] :: [Int])
	--print (safetail6 [1,2,3,4]) 
	----print (safetail7 [] :: [Int])
	----print (safetail7 [1,2,3,4]) 
	--print (safetail8 [] :: [Int])
	--print (safetail8 [1,2,3,4]) 
	
	print "test"

	print (remove 0 [1,2,3,4])

