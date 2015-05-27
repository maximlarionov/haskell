fact' n k = if n > 1
                  then fact' (n-1) (k*n)
                  else k
