import Data.List (foldl')
add = \x y -> x + y --created add function

evenSum list = foldl' add 0 (filter even list)

