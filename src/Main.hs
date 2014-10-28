
-- | Main entry point to the application.
module Main where

-- | The main entry point.
main :: IO ()

n = a `div` (length xs)
    where   a = 10
            xs = [1,2,3,4,5]

last xs = head (reverse xs)

last1 xs = drop (length xs) xs

-- test

main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
    print n
    print (Main.last [1,2,3,4,5])
    print (Main.last1 [1,2,3,4,5])
    print (length [1,2,1])
    print (head)