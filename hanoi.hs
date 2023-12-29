 {-The tower of Hanoi (commonly also known as the "towers of Hanoi"), is a puzzle invented by E. Lucas in 1883. It is also known as the Tower of Brahma puzzle and appeared as an intelligence test for apes in the film Rise of the Planet of the Apes (2011) under the name "Lucas Tower."

Given a stack of n disks arranged from largest on the bottom to smallest on top placed on a rod, together with two empty rods, the tower of Hanoi puzzle asks for the minimum number of moves required to move the stack from one rod to another, where moves are allowed only if they place smaller disks on top of larger disks. The puzzle with n=4 pegs and n disks is sometimes known as Reve's puzzle.

The problem is isomorphic to finding a Hamiltonian path on an n-hypercube (Gardner 1957, 1959). 
-}
hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

--One can use this function to produce output, just like the other programs:

hanoiIO n = mapM_ f $ hanoi n 1 2 3 where
  f (x,y) = putStrLn $ "Move " ++ show x ++ " to " ++ show y

-- Trying Reve's puzzle
reve :: Integer -> a -> a -> a -> a -> [(a,a)]
reve 0 _ _ _ _ = []
reve n a b c d = reve (n-1) a c d c ++ [(a,d)] ++ reve (n-1) c d a d 


-- Output of rave
reveIO n = mapM_ f $ reve n 1 2 3 4 where
  f (x,y) = putStrLn $ "Move " ++ show x ++ " to " ++ show y