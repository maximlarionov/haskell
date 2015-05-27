square :: Num a => a -> a
square = (^2)

absolute' x
    | x >= 0 = x
    | otherwise = -x
