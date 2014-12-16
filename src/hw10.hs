module Main where

import Data.List
import Data.Char
import Unsafe.Coerce
import System.IO

--removeone1 x [] = [x]
--removeone1 x ys
--    | x == head ys = ys
--    | otherwise = y : removeone1 x ys

removeone2 x [] = []    
removeone2 x (y : ys)
    | x == y = ys
    | otherwise = x : removeone2 y ys

removeone3 x [] = []
removeone3 x ys
    | x == head ys = ys
    | otherwise = removeone3 x ys
    
removeone4 x [] = []
removeone4 x (y : ys)
    | x == y = ys
    | otherwise = y : removeone4 x ys

isChoice1 [] _ = True
isChoice1 (x : xs) [] = False
isChoice1 (x : xs) ys = elem x ys && isChoice1 xs (removeone4 x ys)

isChoice2 [] _ = False
isChoice2 (x : xs) [] = True
isChoice2 (x : xs) (y : ys) = elem y xs && isChoice2 xs (removeone4 x ys)

--isChoice3 [] _ = True
--isChoice3 xs [] = True
--isChoice3 xs ys = elem (head xs) ys && isChoice3 xs (removeone4 (head y) ys)

isChoice4 [] _ = True
isChoice4 (x : xs) [] = False
isChoice4 (x : xs) ys = elem x ys && isChoice4 (removeone4 x xs) ys

--split1 :: [a] -> [([a], [a])]
--split1 [] = []
--split1 [x] = [x]
--split1 (x:xs) = [([x] : (ls ++ rs)) | (ls, rs) <- split1 xs]

split2 :: [a] -> [([a], [a])]
split2 [] = []
split2 (x:xs) = ([x], xs) : (split2 xs)

split3 :: [a] -> [([a], [a])]
split3 [] = []
split3 (x:xs) = [(x : ls, rs) | (ls, rs) <- split3 xs]

split4 :: [a] -> [([a], [a])]
split4 [] = []
split4 [_] = []
split4 (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split4 xs]

main = do
    putStrLn "The answers are"
    print (split2 [1,2,3,4])
    print (split3 [1,2,3,4])
    print (split4 [1,2,3,4])
    
    {-
    print (isChoice1 [1,2,3] [1,3,4,2,5])
    print (isChoice2 [1,2,3] [1,3,4,2,5])
    print (isChoice4 [1,2,3] [1,3,4,2,5])
    
    print (isChoice1 [1,2,2,3] [1,3,4,2,5])
    print (isChoice2 [1,2,2,3] [1,3,4,2,5])
    print (isChoice4 [1,2,2,3] [1,3,4,2,5])
    
    --print (isChoice1 [1,2] [1,3,4,5])
    --print (isChoice2 [1,2] [1,3,4,5])
    --print (isChoice4 [1,2] [1,3,4,5])
    
    --print (isChoice1 [1,3,4,5,6] [])
    --print (isChoice2 [1,3,4,5,6] [])
    --print (isChoice4 [1,3,4,5,6] [])
    -}
    {-
    putStrLn (removeone2 '1' "21234")
    --putStrLn (removeone3 '1' "21234")
    putStrLn (removeone4 '1' "21234")
    -}