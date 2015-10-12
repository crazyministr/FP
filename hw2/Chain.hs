-- Малашенков Антон, гр. А3401

module Chain where

-- Написать функцию chain :: Int -> Int -> Double, которая вычисляет
-- приближенное значение цепной дроби a + 1/(a + 1/(a + 1/(a +...))).
-- Первый аргумент функции задает значение числа a, второй - длину дроби (количество сложений)

chain a len
    | len < 0   = error ("len must be >= 0")
    | otherwise = iterate (\x -> a' + 1/x) a' !! len
      where a' = fromIntegral a


-- for testing ...
main = do
    print (chain 1 0)
    print (chain 1 1)
    print (chain 1 2)
    print (chain 10 10)
    print (chain 1 (-3))

