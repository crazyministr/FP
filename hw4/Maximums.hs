module Maximums where

import Data.List

{-
 Написать функцию maximums :: [Integer] -> [Integer], которая по заданному списку целых выдает список
 его локальных максимумов. Локальным максимумом будем называть элемент списка, больший своих соседей -
 следующего и предыдущего элементов. Например в списке [1,3,2,2,4,6,5,5] локальными максимумами являются
 элементы с индексами 1 и 5 и только они, поэтому вызов maximums [1,3,2,2,4,6,5,5] должен выдать в
 качестве результата [3,6] - список элементов с этими индексами.
-}

{-
 К исходному списку применяет функцию zipWith3 и
   - если средний элемент больше соседей, то приводим число к String
   - иначе вставляем "-" (знак, который будет означать что среднее число не больше соседей)
 Я сделал приведению к String для того, чтобы явно применить filter и отбросить всё, что не равно "-"
 После того как мы отфильтровали наш список, нужно произвести конвертацию из String в Integer,
 для этого применяем функцию map.
-}
maximums :: [Integer] -> [Integer]
maximums a
    | length a < 3 = error "length of list must be > 2"
    | otherwise = map(\x -> read x::Integer) $
                  filter (\x -> x /= "-") $
                  zipWith3 (\x y z -> if x < y && y > z then (show y) else "-")
                      a (tail a) (tail (tail a))

