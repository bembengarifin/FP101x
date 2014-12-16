module Main where

double x = x + x

quadruple x = double (double x)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where lesser = filter (<= x) xs
          greater = filter (> x) xs
          
main = do
    print "Hello World"
    print (double 2)
    print (quadruple 3)
    print (quicksort [4,6,3,2,4,7])
    