stringToInt :: [Char]->Integer
-- String To Int
stringToInt x = read x :: Integer
-- Integer to word
maps f l = foldr (\x xs -> (f x):xs) [] l
filters f l = foldr (\x xs -> if f x then x:xs else xs) [] l
word1 x = case x of
    0 -> ""
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
word2 x = case x of
    2 -> "twenty"
    3 -> "thirty"
    4 -> "forty"
    5 -> "fifty"
    6 -> "sixty"
    7 -> "seventy"
    8 -> "eighty"
    9 -> "ninety"
word x = if(x<20)
		 then word1 x
		 else if(x>=20 && x<100)
		 then (word2 (x `div` 10)++" "++word1 (x `mod` 10)) 
		 else if(x `mod` 100 /= 0)
		 then (word1 (x `div` 100)++" "++"hundred"++" "++"and"++" "++word (x `mod` 100))
		 else (word1 (x `div` 100)++" "++"hundred")
numWord 0 = "zero"		 
numWord x = if(x<1000)
				 then word x
				 else if(x>=1000 && x<1000000 && x `mod` 1000 /= 0)
				 then (word ( x `div` 1000)++" thousand,"++" and "++word( x `mod` 1000))
				 else if(x>=1000 && x<1000000 && x `mod` 1000 == 0)
				 then (word ( x `div` 1000)++" thousand")
				 else if(x>=1000000 && x<1000000000 && (x `div` 1000)`mod`1000 /= 0 && x `mod` 1000 /= 0)
				 then (word (x `div` 1000000)++" million, "++word (( x `div` 1000)`mod` 1000)++" "++"thousand,"++" and "++word( x `mod` 1000))
				 else if(x>=1000000 && x<1000000000 && (x `div` 1000)`mod`1000 /= 0 && x `mod` 1000 == 0)
				 then (word (x `div` 1000000)++" million, and "++word (( x `div` 1000)`mod` 1000)++" "++"thousand")
				 else if(x>=1000000 && x<1000000000 && (x `div` 1000)`mod`1000 == 0 && x `mod` 1000 /= 0)
				 then (word (x `div` 1000000)++" million, and "++word( x `mod` 1000))
				 else if(x>=1000000 && x<1000000000 && (x `div` 1000)`mod`1000 == 0 && x `mod` 1000 == 0)
				 then (word (x `div` 1000000)++" million")
				 else "number too big"
-- Word to Int
wordNum b = [ a | a <-[0..999999], numWord a == b] 			 
