module SqFib where

{-
 Написать выражение для вычисления значения sqFib :: [Integer], представляющего собой бесконечный список
 натуральных чисел, расположенных в порядке возрастания значений, начинающийся с единицы и
 содержащий все значения вида (n^2 - 1) и числа Фибоначчи.
 Начало такого списка: [1,2,3,5,8,13,15,21,24,34,...
-}

import Data.List

-- сливаем два списка в один и удаляем одинаковые элемента из результирующего списка
sqFib :: [Integer]
sqFib = nub $ merge [x * x - 1 | x <- [2..]] [x | x <- fibs]

-- генерация чисел Фибоначчи
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- merge двух списков
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = if x < y
                         then x:(merge xs (y:ys))
                         else y:(merge (x:xs) ys)
