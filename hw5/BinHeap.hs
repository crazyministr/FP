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

import Data.List

data BinHeap e = BinHeap [e] deriving (Show)

extract :: Ord e => BinHeap e -> (e, BinHeap e)
extract (BinHeap [e]) = (e, BinHeap [])
extract (BinHeap e) = (head e, heapify (BinHeap ([last e] ++ (tail $ init e))) 0)

{-
 Функция принимает 2 параметра - кучу и текущую позицию в ней,
 и придаёт бинарной куче её свойства
 Спускаемся начиная с корня и если
   - левый  ребёнок < элемента в текущей вершине и < правого, то меняем их местами, и продолжаем спускатся
   - правый ребёнок < элемента в текущей вершине, то меняем их местами, и продолжаем спускатся
   - иначе возвращаем текущую кучу

 Если левого или правого ребёнка не существует, то можно им временно выдать значение в текущей вершине
 Это ничего не изменит, т.к. используется строгое сравнение
-}
heapify :: Ord e => BinHeap e -> Int -> BinHeap e
heapify (BinHeap e) pos
    | leastChild /= pos = heapify (swapAt (BinHeap e) pos leastChild) leastChild
    | otherwise = BinHeap e
      where left  = pos * 2 + 1
            right = pos * 2 + 2

            posValue = e !! pos
            leftValue =  if length e > left  then e !! left  else e !! pos
            rightValue = if length e > right then e !! right else e !! pos
            leastChild =
                if posValue > leftValue && leftValue < rightValue
                    then left
                else if posValue > rightValue
                    then right
                else pos

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

-- testing
main = do
    print $ unfoldr (\t -> case t of
                    BinHeap [] -> Nothing
                    BinHeap list -> Just (extract t)) (BinHeap [1..10])
