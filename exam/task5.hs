data Nat = Zero | Succ Nat
                          deriving (Eq, Ord, Show)

instance Num Nat where
  Zero + y = y
  Succ x + y = Succ(x + y)

Succ x - y =
                case x of
                Zero -> y
                Succ xx -> case y of
                                  Zero -> x + 1
                                  Succ yy -> x - yy
                                  Zero * y = Zero
                                  Succ x * y = y + (x * y)

fromInteger 0 = Zero
fromInteger n = Succ(fromInteger(n-1))

toInt :: Nat -> Integer
toInt n = case n of
        Zero -> 0
        Succ m -> (1 + (toInt m))

zero = Zero
one = Succ $ Zero
two = Succ one
three = Succ two
four = Succ three
