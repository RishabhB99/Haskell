import Data.Char
import Debug.Trace
bitsum :: [Int] -> [Int] -> [Int]
shifter l = [head(l)] ++ init(l)
bitsum' 0 (x:xs) (y:ys) = 1 : bitsum' 0 xs ys
bitsum' 1 (x:xs) (y:ys) = 0 : bitsum' 1 xs ys

rev l = if null l then l else rev(tail(l)) ++ [head(l)]

bitsum  = bitsum' 0
  where
    bitsum' _ [] [] = []
    bitsum' carry (x:xs) (y:ys) | x == y = carry : bitsum' x xs ys
                                | otherwise = (1-carry) : bitsum' carry xs ys
								
bsum l1 l2 = rev(bitsum (rev(l1)) (rev(l2)))		
list n =[fst('0',i) | i<-[0..n]]
zero n =[fst(0,i) | i<-[0..n-1]]
f1 l = [digitToInt i|i<-l]
f2 [] = []
f2 (x:xs) | x==0 = '0':f2(xs)
			| otherwise = '1':f2(xs)
inverse [] = []
inverse (x:xs)  | x=='0' = '1':inverse(xs)
				| otherwise = '0':inverse(xs)
complement x n = f2 (bsum (f1 (inverse x)) ((zero (n-1)) ++ [1]))

booths l1 l2 l3 n n1= if(n==0)
				then trace ("Ans = ") (init l3)
				else if(l3!!(2*n1-1)==1 && l3!!(2*n1)==0)
				then do 
					trace ("A = "++show l1++" S = "++show l2++" P = "++show l3) (booths l1 l2 (shifter(bsum l2 l3)) (n-1) n1)
				else if(l3!!(2*n1-1)==0 && l3!!(2*n1)==1)
				then trace ("A = "++show l1++" S = "++show l2++" P = "++show l3) (booths l1 l2 (shifter(bsum l1 l3)) (n-1) n1)
				else do
					trace ("A = "++show l1++" S = "++show l2++" P = "++show l3) (booths l1 l2 (shifter l3) (n-1) n1)
					
main = do
	print("Enter the number of bits")
	input1<-getLine
	let n = (read input1 :: Int)
	print("Enter the two numbers")
	x<-getLine
	y<-getLine
	print(booths (f1 (x ++ (list n))) (f1 ((complement x n) ++ (list n))) (f1 ((list (n-1)) ++ y ++ ['0'])) n n)