main = do
  S <- getLine
  let i = read S :: Int
  let f = factorial i
  putStrLn $ show f
