infixr 5 :::
data List a = Nil | a ::: (List a)
                    deriving (Show, Read, Eq, Ord)
                    -- print, read, equal, bigger-lesser
