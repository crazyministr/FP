module Square where

{-
 Написать выражение для вычисления бесконечной матрицы square :: [[Integer]],
 которая содержит все натуральные числа, начиная с единицы, расположенные “в квадратик”
 как показано на схеме:

 1  2  5  10 ...
 4  3  6  11 ...
 9  8  7  12 ...
-}

square :: [[Integer]]
square = map genRow $ zip3 (row 1 3) ([0..]) ([1, 3..])

-- функция genRow возвращает очередную строку матрицы
--     firstElem: первый элемент текущей строки
--     counDec:   кол-во первых убывающих элементов
--     diff:      разница между последним из убывающих и следующим элеметнами строки
-- для того, чтобы задать список из убывающих элементов, нужно задать как минимум 2 первых элемента
-- поэтому есть проверки на countDec == 0 и countDec == 1
genRow :: (Integer, Integer, Integer) -> [Integer]
genRow (firstElem, countDec, diff)
    | countDec == 0 = row firstElem diff
    | countDec == 1 = [firstElem] ++ (row (firstElem - 1) diff)
    | otherwise = [firstElem, (firstElem - 1) .. (firstElem - countDec + 1)] ++
      (row (firstElem - countDec) diff)

-- функция row возвращает бесконечный список возврастрающих элементов таких, что
-- каждый следующий элемент больше предущего на diff + 2
--     value: текущее значение
--     diff:  разница между текущим и следующим элементами списка
row :: Integer -> Integer -> [Integer]
row value diff = value : row (value + diff) (diff + 2)

main = do
    print $ map (take 5) (take 5 square)

