fib :: Int -> Int
prime :: Int -> Int
fibs :: [Int]
primes :: [Int]
fib 1 = 1
fib 2 = 1
fib n = fib(n-1)+fib(n-2)
fibs = [fib n | n<-[1..]]
primes = [n | n<-[2..], not $ elem n [j*k | j<-[2..n-1], k<-[2..n-1]]] 
prime n = primes !! (n-1)