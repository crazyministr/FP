module CountByPrefix where

{-
 Написать функцию countByPrefix :: String -> Dictionary -> Int,
 которая подсчитывает количество слов в словаре с заданным префиксом.
-}

{- добавил для себя deriving (Show, Eq) для того, чтобы можно было
       - распечатать дерево
       - возможности сравнения (x /= Empty)
-}
data Trie = Empty | Node Char [Trie] deriving (Show, Eq)
type Dictionary = [Trie]

-- Принимаем исходные данные и, для подсчёта ответа, вызываем функцию countByPrefix', передавая туда Node
countByPrefix :: String -> Dictionary -> Int
countByPrefix prefix trie = countByPrefix' (prefix) (Node ' ' trie)

{-
 данная функция спускается по префиксному дереву вниз до тех пор, пока незакончится исхдный префикс или
 не найдётся его продолжение
-}
countByPrefix' :: String -> Trie -> Int
countByPrefix' (p:prefix) (Node _ trie)
    | length nextSubTrie > 0 = countByPrefix' prefix nextNode
    | otherwise = 0
      where nextSubTrie = filter(\(Node v _) -> v == p) trie
            nextNode = head nextSubTrie

{-
 если исходный префикс закончился, значит он был полностью найден в дереве и теперь осталось посчитать
 сколько слов заканчивается в поддереве. Т.е. просто спускаемся по дереву если видим Node,
 иначе +1 к ответу
-}
countByPrefix' [] (Node _ trie) = sum $ map (\x -> if x /= Empty then countByPrefix' [] x else 1) trie

-- some testing
main = do
    -- bit, byte, bite, site
    let trie = [Node 'b' [Node 'i' [Node 't' [Empty,
                                              Node 'e' [Empty]]],
                          Node 'y' [Node 't' [Node 'e' [Empty]]]],
                Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]]

    print $ countByPrefix "b" trie == 3
    print $ countByPrefix "bi" trie == 2
    print $ countByPrefix "site" trie == 1
    print $ countByPrefix "" trie == 4
    print $ countByPrefix "a" trie == 0

