insertAt :: Char -> [Char] -> Int -> [Char]
incrrange :: [Int] -> (Int,Int) -> [Int]
uniq :: [Char] -> [Char]
combinations :: Int -> [Char] -> [[Char]]
insertAt c l n = if(n>0 && n<=length(l)+1)
then take (n-1) l ++ [c] ++ drop (n-1) l
else "error"
incrrange l t=take (fst t -1) l ++ [(l!!(n-1))+1 | n<-[fst t..snd t]] ++ drop (snd t) l
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)
combinations 0 _ = [[]]
combinations n l = let xs = uniq l
                   in [ xs !! i : x | i <- [0..(length xs)-1] , x <- combinations (n-1) (drop (i+1) xs) ]
