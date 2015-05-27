multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


filter_regular :: (a -> Bool) -> [a] -> [a]
filter_regular _ [] = []
filter_regular f (x:xs) = case (f x) of
                                      True -> x:(filter_regular f xs)
                                      False -> (filter_regular f xs)


getIndex :: [a] -> Int -> a
getIndex _ [] = []
getIndex f (x:xs)


