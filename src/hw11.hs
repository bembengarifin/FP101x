module Main where

test = (\x -> 1 + x) (2 * 3)

fibs1 = 1 : [x + y | (x,y) <- zip fibs1 (tail fibs1)]
fibs2 = 0 : 1 : zipWith (*) fibs2 (tail fibs2)
fibs3 = 0 : 1 : [x + y | (x,y) <- zip fibs3 (tail fibs3)]
fibs4 = 1 : 1 : [x + y | (x,y) <- zip (tail fibs4) fibs4] 
          
main = do
    print "Hello World"
    print (test)
    --print (take 10 fibs1)
    print (take 20 fibs2)
    print (take 20 fibs3)
    print (take 10 fibs4)
    
    print (fibs3 !! 7)
    
    print (head (dropWhile (<=1000) fibs3))
    print (last (take 19 fibs3))
    --print (filter (> 1000) fibs3)
    print (head (drop 1000 fibs3))
    
    print (take 6 (repeat 10))
    
    