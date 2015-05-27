fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2  )

fibs = take 10 $ 0 : 1 : zipWith (+) fibs (tail fibs)

--zipwith (+) [1,2,3] [1, 2, 3] => [2, 4, 6]
-- beforelast fib + last fib by zipWith
-- 1:1 -> 1:1:2 -> 1:1:2:3 -> 1:1:2:3:5
