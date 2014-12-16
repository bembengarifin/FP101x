module Main where

import Data.List
import Data.Char
import Unsafe.Coerce
import System.IO

data Nat = Zero
		 | Succ Nat
		 deriving Show
		 
natToInteger1 Zero = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

natToInteger2 (Succ n) = natToInteger2 n + 1
natToInteger2 Zero = 0

natToInteger3 n = natToInteger3 n

natToInteger4 (Succ n) = 1 + natToInteger4 n 
natToInteger4 Zero = 0

natToInteger5 Zero = 1
natToInteger5 (Succ n) = (1 + natToInteger5 n) + 1 

natToInteger6 = head . m
    where m Zero = [0]
          m (Succ n) = [sum [ x | x <- (1 : m n)]]

natToInteger7 :: Nat -> Integer
natToInteger7 = \ n -> genericLength [ c | c <- show n, c == 'S']

--natToInteger8 :: Nat -> Integer
--natToInteger8 = \ n -> length [ c | c <- show n, c == 'S']
          
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

integerToNat2 0 = Succ Zero
integerToNat2 n = (Succ (integerToNat2 n))
      
integerToNat3 n = product [(unsafeCoerce c) :: Integer | c <- show n]

integerToNat4 n = integerToNat4 n      

integerToNat5 (n + 1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

integerToNat6 (n + 1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero

integerToNat7 = head . m
    where {
          ; m 0 = [0]
          ; m (n + 1) = [sum [ x | x <- (1 : m n)]]
          }

--integerToNat8 :: Integer -> Nat
--integerToNat8 = \ n -> genericLength [ c | c <- show n, isDigit c]
          
add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)         

add2 (Succ m) n = Succ (add2 n m)  
add2 Zero n = n

add3 Zero n = Zero
add3 (Succ m) n = Succ (add3 m n)  

add4 (Succ m) n = Succ (add4 m n)
add4 Zero n = Zero

add5 n Zero = Zero
add5 n (Succ m) = Succ (add5 n m)  

add6 n (Succ m) = Succ (add6 n m)  
add6 n Zero = Zero

add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)  

add8 n (Succ m) = Succ (add8 m n)  
add8 n Zero = n

--mult1 Zero Zero = Zero
--mult1 m (Succ n) = add1 m (mult1 m n)

mult2 m Zero = Zero
mult2 m (Succ n) = add1 m (mult2 m n)

mult3 m Zero = Zero
mult3 m (Succ n) = add1 n (mult3 m n)

mult4 m Zero = Zero
mult4 m n = add1 m (mult4 m (Succ n))

data Tree = Leaf Integer
          | Node Tree Integer Tree
          deriving Show

occurs0 :: Integer -> Tree -> Bool
occurs0 m (Leaf n) = m==n  
occurs0 m (Node l n r) = m==n
                         || occurs0 m l
                         || occurs0 m r

occurs1 :: Integer -> Tree -> Bool
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r) = case compare m n of
                            LT -> occurs1 m l
                            EQ -> True
                            GT -> occurs1 m r                                

occurs2 :: Integer -> Tree -> Bool                            
occurs2 m (Leaf n) = m == n
occurs2 m (Node l n r) = case compare m n of
                            LT -> occurs2 m r
                            EQ -> True
                            GT -> occurs2 m l                                

--occurs3 :: Integer -> Tree -> Bool                            
--occurs3 m (Leaf n) = compare m n
--occurs3 m (Node l n r) = case compare m n of
--                            LT -> occurs3 m l
--                            EQ -> True
--                            GT -> occurs3 m r                                

occurs4 :: Integer -> Tree -> Bool     
occurs4 m (Leaf n) = m == n
occurs4 m (Node l n r) = case compare m n of
                            LT -> occurs4 m l
                            EQ -> False
                            GT -> occurs4 m r                                
                            
occurs5 :: Integer -> Tree -> Bool                                 
occurs5 m (Leaf n) = m == n
occurs5 m (Node l n r) 
    | m == n = True
    | m < n = occurs5 m l
    | otherwise = occurs5 m r

occurs6 :: Integer -> Tree -> Bool         
occurs6 m (Leaf n) = m == n
occurs6 m (Node l n r) 
    | m == n = True
    | m > n = occurs6 m l
    | otherwise = occurs6 m r

--occurs7 :: Integer -> Tree -> Bool         
--occurs7 m n = m == n
--occurs7 m (Node l n r) 
--    | m == n = True
--    | m < n = occurs7 m l
--    | otherwise = occurs7 m r

--occurs8 :: Integer -> Tree -> Bool         
--occurs8 m n = m == n
--occurs8 m (Node l n r) 
--    | m == n = True
--    | m < n = occurs8 m r
--    | otherwise = occurs8 m l    

data Tree1 = Leaf1 Integer
          | Node1 Tree1 Tree1
          deriving Show
          
leaves1 (Leaf1 x) = x
leaves1 (Node1 l r) = leaves1 l + leaves1 r
balanced1 :: Tree1 -> Bool
balanced1 (Leaf1 _) = True
balanced1 (Node1 l r) = abs (leaves1 l - leaves1 r) <= 1 || balanced1 l || balanced1 r

--leaves2 (Leaf1 _) = True
--leaves2 (Node1 l r) = leaves2 l + leaves2 r
--balanced2 :: Tree1 -> Bool
--balanced2 (Leaf1 _) = True
--balanced2 (Node1 l r) = abs (leaves2 l - leaves2 r) <= 1 

--leaves3 (Leaf1 _) = True
--leaves3 (Node1 l r) = leaves3 l + leaves3 r
--balanced3 :: Tree1 -> Bool
--balanced3 (Leaf1 _) = True
--balanced3 (Node1 l r) = abs (leaves3 l + leaves3 r) <= 1 

leaves4 (Leaf1 _) = 1
leaves4 (Node1 l r) = leaves4 l + leaves4 r
balanced4 :: Tree1 -> Bool
balanced4 (Leaf1 _) = True
balanced4 (Node1 l r) = abs (leaves4 l - leaves4 r) <= 1 && balanced4 l && balanced4 r 

halve1 xs = splitAt (length xs `div` 2) xs
balance1 [x] = Leaf1 x
balance1 xs = Node1 (balance1 ys) (balance1 zs) 
    where (ys, zs) = halve1 xs

--halve2 xs = splitAt (length xs / 2) xs
--balance2 [x] = Leaf1 x
--balance2 xs = Node1 (balance2 ys) (balance2 zs) 
--    where (ys, zs) = halve2 xs

--halve3 xs = splitAt (length xs `div` 2) xs
--balance3 [x] = Leaf1 x
--balance3 xs = Node1 ys zs
--    where (ys, zs) = balance3 (halve3 xs)

--halve4 xs = splitAt (length xs `div` 2) xs
--balance4 x = Leaf1 x
--balance4 xs = Node1 (balance4 ys) (balance4 zs) 
--    where (ys, zs) = halve4 xs    

data Expr = Add Expr Expr
          | Val Int
          deriving Show
          
main = do
    putStrLn "The answers are"
    print (Add (Val 1) (Val 2))
    --print (balance1 [1,2,3,4,5])
    {-
    let t1 = (Node1 (Node1 (Leaf1 1) (Leaf1 4)) (Node1 (Leaf1 6) (Leaf1 9)))
    let t2 = (Node1 (Node1 (Node1 (Leaf1 1) (Node1 (Leaf1 2) (Leaf1 3))) (Leaf1 4)) (Node1 (Leaf1 6) (Leaf1 9)))
    let t3 = (Node1 (Node1 (Leaf1 1) (Leaf1 4)) (Node1 (Leaf1 6) (Node1 (Leaf1 9) (Node1 (Leaf1 10) (Leaf1 11)))))
    let t4 = (Node1 (Leaf1 1) (Node1 (Leaf1 6) (Leaf1 9)))
    let t5 = (Node1 (Node1 (Leaf1 1) (Leaf1 4)) (Leaf1 6))
    print (balanced1 t1)
    print (balanced1 t2)
    print (balanced1 t3)
    print (balanced1 t4)
    print (balanced1 t5)
    print (balanced4 t1)
    print (balanced4 t2) 
    print (balanced4 t3) 
    print (balanced4 t4) 
    print (balanced4 t5)
    -}
{-
    let t = (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))
    --print (occurs0 5 t)
    print (occurs1 3 t)
    print (occurs2 3 t)
    --print (occurs3 5 t)
    print (occurs4 3 t)
    print (occurs5 3 t)
    print (occurs6 3 t)
    --print (occurs7 5 t)
    --print (occurs8 5 t)
-}        
    --print (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))
    {-
    --print (natToInteger1 (mult1 (Succ (Succ (Succ Zero))) (Succ(Succ Zero)))) 
    print (natToInteger1 (mult2 (Succ (Succ (Succ Zero))) (Succ(Succ Zero)))) 
    print (natToInteger1 (mult3 (Succ (Succ (Succ Zero))) (Succ(Succ Zero)))) 
    --print (natToInteger1 (mult4 (Succ (Succ (Succ Zero))) (Succ(Succ Zero)))) 
    print (natToInteger1 (Succ (Succ (Succ Zero))) * natToInteger1 (Succ (Succ Zero)))
    -}          
    {-
    print (natToInteger1 (add1 (Succ (Succ (Succ Zero))) (Succ(Succ Zero)))) 
    print (natToInteger1 (add2 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    print (natToInteger1 (add3 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    print (natToInteger1 (add4 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    print (natToInteger1 (add5 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    print (natToInteger1 (add6 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    print (natToInteger1 (add7 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    print (natToInteger1 (add8 (Succ (Succ (Succ Zero))) (Succ(Succ Zero))))
    
    print (natToInteger1 (Succ (Succ (Succ Zero))) + natToInteger1 (Succ (Succ Zero)))
    -}
    {-
    print (integerToNat1 10)
    --print (integerToNat2 10)
    print (integerToNat3 3)
    --print (integerToNat4 3)
    print (integerToNat5 3)
    print (integerToNat6 3)
    print (integerToNat7 3)
    --print (integerToNat8 3)
    -}
    {-
    print (natToInteger1 (Succ (Succ Zero)))
	print (natToInteger1 (Succ Zero))
	print (natToInteger2 (Succ (Succ Zero)))
	print (natToInteger2 (Succ Zero))
	--print (natToInteger3 (Succ (Succ Zero)))
	--print (natToInteger3 (Succ Zero))
	print (natToInteger4 (Succ (Succ Zero)))
	print (natToInteger4 (Succ Zero))
	print (natToInteger5 (Succ (Succ Zero)))
	print (natToInteger5 (Succ Zero))
	print (natToInteger6 (Succ (Succ Zero)))
	print (natToInteger6 (Succ Zero))
	print (natToInteger7 (Succ (Succ Zero)))
	print (natToInteger7 (Succ Zero))
	--print (natToInteger8 (Succ (Succ Zero)))
	--print (natToInteger8 (Succ Zero))
    -}	
