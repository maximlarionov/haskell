-- mapl :: (b -> a) -> [b] -> [a]
-- mapl f = foldl (\x xs -> f x : xs) []

mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\x xs -> f x : xs) []

-- compare:
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- map :: (a -> b) -> [a] -> [b]

-- usage
-- foldr (+) val [list] => sum(list_el + val)

-- passing into foldr lambda function,
--
