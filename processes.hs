type PID = Integer

data Process = Process PID [Process] deriving (Show)

-- tree nodes.
--      (1, -1)
--     /       \
--   (2, 1)     (3, 1)
--                    \
--                     (4, 3)

-- 1 элемент - значение, 2ой - линк на предыдущий узел.

processes = [(1,-1), (3, 1), (2, 1), (4, 3)]
-- функция, которая возвращает PID процесса
pid :: Process -> PID
pid (Process p _) = p
-- функция определяющая фильтрацию
filterOn :: (PID -> PID -> Bool) -> PID -> [(PID, PID)] -> [(PID, PID)]
filterOn f v = filter (\(_, y) -> f v y)
-- сокращения
filterEqual = filterOn (==)
filterNotEqual = filterOn (/=)
-- просто сокращение
mapFst = map fst

buildTree' :: Process -> [(PID, PID)] -> Process
buildTree' proc procs =
  -- получаем всех детей из исходного массива [(1,-1), (2,1), (3,1)]
  -- для proc = Process 1 [] результатом будет такой массив [2, 3]
  let children = mapFst $ filterEqual (pid proc) procs
  -- говорим верни такой же процесс, но теперь для каждого потомка
  -- построй такое же дерево, но уже на отфильтрованных данных то есть на массиве [(1,-1)]
  -- для proc = Process 1 [] результатом будет такой массив [(1, -1)]
  -- таким образом мы входим в рекурсию, которая на каждой итерации
  -- будет уменьшать размер исходного массива пар процессов
  in Process (pid proc) $ map (\c -> buildTree' (Process c []) $ filterNotEqual (pid proc) procs) children


-- просто сокращаем запись и используем эту функцию для построения дерева
buildTree :: [(PID, PID)] -> Process
buildTree = buildTree' (Process 1 [])




----------  -1 ----
-------1 ----------
--- 3----2---------
----4--------------
