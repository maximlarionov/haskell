reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' xs = last xs : reverse' (init xs)

-- last : takes last item
-- init : takes list without last item
-- simply go through list
