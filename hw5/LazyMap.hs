module LazyMap where

{-
 Требуется выбрать представление и определить тип данных
 data Map key value = ..
 для которого требуется реализовать стандартный набор методов для манипуляций с “отображением”:

 get :: Eq k => k -> Map k v -> Maybe v
 put :: Eq k => (k, v) -> Map k v -> Map k v
 remove :: Eq k => k -> Map k v -> Map k v
 keys :: Map k v -> [k]
 values :: Map k v -> [v]

 Написать функцию valuesBy :: (k -> Bool) -> Map k v -> [v], которая выдает список тех значений
 пар отображения, ключи которых удовлетворяют критерию, заданному первым аргументом функции.
 Например, вызов valuesBy (<0) m  выдаст те значения пар отображения m, ключи которых меньше нуля.
-}

import Data.Maybe

data Map key value = Tip | MapPair (key, value) (Map key value) deriving (Show)

-- принимает ключ и Map
-- возвращает значение по заданному ключу или Nothing, если ключ не был найден
get :: Eq k => k -> Map k v -> Maybe v
get key Tip = Nothing
get key (MapPair pair nextMapPair)
    = case key == (fst pair) of
        True -> Just (snd pair)
        False -> get key nextMapPair

-- принимает пару (key, value) и Map.
-- возращает новый Map, в который была вставлена переданная пара (key, value)
-- если переданный ключ уже существовал, то заменяем значение
put :: Eq k => (k, v) -> Map k v -> Map k v
put p Tip = MapPair p Tip
put p (MapPair pair nextMapPair)
    = case (fst p) == (fst pair) of
        True -> MapPair p nextMapPair
        False -> MapPair pair (put p nextMapPair)

-- принимает ключ и Map
-- возвращает новый Map, в котором была удалена пара (key, value)
-- если ключ не был найден, то возвращает тот же Map
remove :: Eq k => k -> Map k v -> Map k v
remove key Tip = Tip
remove key (MapPair pair nextMapPair)
    = case key == (fst pair) of
        True -> nextMapPair
        False -> MapPair pair (remove key nextMapPair)

-- принимает Map
-- возвращает все его ключи
keys :: Map k v -> [k]
keys m = [k | (k, _) <- mapToList m]

-- принимает Map
-- возвращает все его значения
values :: Map k v -> [v]
values m = [v | (_, v) <- mapToList m]

-- принимает Map
-- преобразует переданный Map в list (используя левую свёртку) и возвращает его
mapToList :: Map k v -> [(k, v)]
mapToList m = foldlWithKey (\xs k v -> (k, v):xs) [] m

-- своя функция левой свёртки
foldlWithKey :: (xs -> k -> v -> xs) -> xs -> Map k v -> xs
foldlWithKey _ xs Tip = xs
foldlWithKey func xs (MapPair p m) = func (foldlWithKey func xs m) (fst p) (snd p)

-- принимает функцию и Map
-- возвращает список тех значений пар отображения, ключи которых удовлетворяют критерию,
-- заданному первым аргументом функции.
valuesBy :: (k -> Bool) -> Map k v -> [v]
valuesBy func m = [v | (k, v) <- mapToList m, func k]

