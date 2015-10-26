module BinHeap where

{-
 Двоичная куча определяется как список элементов, в котором на вершине кучи находится элемент
 с индексом 0 (первый элемент списка - наименьший элемент), а непосредственные потомки каждого
 элемента с индексом k есть элементы с индексами (2k+1) и (2k+2), если, конечно, таковые имеются.
 Как и положено в двоичной куче, все потомки каждого элемента больше или равны этому элементу.
 data BinHeap e = BinHeap [e]

 Написать функцию extract :: Ord e => BinHeap e -> (e, BinHeap e), которая удаляет минимальное значение
 из кучи и выдает пару из удаленного значения и модифицированной кучи.
-}

data BinHeap e = BinHeap [e] deriving (Show)

extract :: Ord e => BinHeap e -> (e, BinHeap e)
extract (BinHeap e) = (head e, heapify (BinHeap ([last e] ++ (tail $ init e))) 0)

{-
 Функция принимает 2 параметра - кучу и текущую позицию в ней,
 и придаёт бинарной куче её свойства
 Спускаемся начиная с корня и если
   - левый  ребёнок меньше текущего, то меняем их местами, и продолжаем спускатся
   - аналогично с правым ребёнком
   - иначе возвращаем текущую кучу
-}
heapify :: Ord e => BinHeap e -> Int -> BinHeap e
heapify (BinHeap e) pos
    | length e > left  && e !! pos > e !! left  = heapify (swapAt (BinHeap e) pos left)  left
    | length e > right && e !! pos > e !! right = heapify (swapAt (BinHeap e) pos right) right
    | otherwise = BinHeap e
      where left  = pos * 2 + 1
            right = pos * 2 + 2

{-
 Функция принимает 3 параметра: кучу и два её элемента, которые нужно поменять местами
 Формально, мы имеем обычный list, в котором нужно поменять местами i-ый и j-ый элементы
-}
swapAt :: Ord e => BinHeap e -> Int -> Int -> BinHeap e
swapAt (BinHeap e) i j = BinHeap (leftPart ++ [elemJ] ++ middlePart ++ [elemI] ++ rightPart)
    where elemI = e !! i
          elemJ = e !! j
          leftPart = take i e
          middlePart = take (j - i - 1) (drop (i + 1) e)
          rightPart = drop (j + 1) e

