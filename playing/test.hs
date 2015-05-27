binomial :: Int -> Int -> Integer
binomial n k = binomialTab!!n!!k

binomialTab :: [[Integer]]
binomialTab = [[ binomialCalc n k | k <- [0..n]] | n <- [0..]]

binomialCalc :: Int -> Int -> Integer
binomialCalc n k
  | k == 0 || k == n = 1
  | k == 1           = toInteger n
  | otherwise        = binomial (n-1) (k-1) + binomial (n-1) k
