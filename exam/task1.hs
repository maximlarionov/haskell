getElement' :: Int -> [a] -> a
getElement' _ [] = Nothing
getElement' n (x:xs)
    | (n == 1) = x
    | otherwise = getElement' (n-1) xs
