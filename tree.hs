import Data.List

data BinTree a = Empty
                 | Node a (BinTree a) (BinTree a)
                              deriving (Eq, Ord)

-- говорим что  BinTree будет экземпляром Show
instance (Show a) => Show (BinTree a) where
  -- перед корнем будет отображаться '<'
  -- и напишем : в начале строки
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    -- treeshow pref Tree
    --   отображает дерево и начинает каждую строку с  pref
    -- Мы не будем отображать пустое дерево
    treeshow pref Empty = ""
    -- Leaf
    treeshow pref (Node x Empty Empty) =
                  (pshow pref x)

    -- Правая ветка пустая
    treeshow pref (Node x left Empty) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " left)

    -- Левая ветка пустая
    treeshow pref (Node x Empty right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- Дерево с непустыми правой и левой ветками
    treeshow pref (Node x left right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|--" "|  " left) ++ "\n" ++
                  (showSon pref "`--" "   " right)

    -- отображение дерева с  красивыми префиксами
    showSon pref before next t =
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow заменяет "\n" на "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (show x)

    -- заменяет символ строкой
    replace c new string =
      concatMap (change c new) string
      where
          change c new x
              | x == c = new
              | otherwise = x:[] -- "x"

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
                             (treeFromList (filter (>x) xs))
