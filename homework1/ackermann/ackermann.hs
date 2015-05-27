    ack :: Int -> Integer -> Integer
    ack 0 n = n + 1
    ack 1 n = n + 2
    ack 2 n = 2 * n + 3
    ack m 0 = ack (m-1) 1
    ack m n = ack (m-1) (ack m (n-1) )
