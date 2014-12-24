module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action :: Concurrent a -> Action
--action = error "You have to implement action"
--action :: ((a -> Action) -> Action) -> Action
--action = error "You have to implement action"
action (Concurrent f) = f (\a -> Stop)


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
--stop = error "You have to implement stop"
stop = Concurrent (\a -> Stop)


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
--atom = error "You have to implement atom"
atom io = Concurrent (\b -> Atom(io >>= \a -> return (b a)))


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
--fork = error "You have to implement fork"
fork f = Concurrent (\a -> Fork(action f) (a ()))

par :: Concurrent a -> Concurrent a -> Concurrent a
--par = error "You have to implement par"
par (Concurrent a) (Concurrent b) = Concurrent (\c -> Fork (a c) (b c))

-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = Concurrent (\c -> f (\a -> case g a of (Concurrent b) -> b c))
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
--roundRobin = error "You have to implement roundRobin"
roundRobin [] = return ()
roundRobin (Atom io : xs) = io >>= \a -> roundRobin (xs ++ [a])
roundRobin (Fork a b : xs) = roundRobin (xs ++ [a,b])
roundRobin (Stop : xs) = roundRobin xs

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- =================((a -> Action) -> Action) -> Action((a -> Action) -> Action) -> Action==================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

