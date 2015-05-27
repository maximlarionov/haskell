reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' xs = last xs : reverse' (init' xs)

init' :: [a] -> [a]
init' []  = error("This is not right, empty list is not allowed here")
init' (x:[]) = [x]
init' (x:xs:[]) = [x]
init' (x:xs) = x: init' xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' (x:[]) =  Just x
last' (x:xs) = last' xs

-- last : takes last item
-- init : takes list without last item
-- simply go through list
--  list can be anything
