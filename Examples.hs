module Examples where

--sum' :: (Num a) => [a] -> a
--sum' = foldr (+) 0
sum' [] = 0
sum' ((:) 0 b) = sum' b
sum' (a:blablabla) = a + (sum' blablabla)

data Abc = Abc1 Int Double | Abc2 String

(-&-) f g x = (f x) && (g x)
infixr 3 -&-

f = (< 10) -&- (\x -> x `mod` 2 == 0)

abc :: Abc -> [Char]
abc (Abc1 0 _) = "Yoyo"
abc (Abc1 i d) = "Abc " ++ (show i) ++ " " ++ (show d)
abc (Abc2 s) = "Abc2 " ++ s

--chain :: Int -> Int -> (Double, Double)
chain :: Int -> Int -> Double
chain' :: Int -> Int -> Double

chain a len
    | len < 0   = error ("len must be >= 0")
    | otherwise = fromIntegral a + recip (fromIntegral a + chain' a (len - 1))

chain' a len
    | len == 0  = 0
    | otherwise = recip (fromIntegral a + chain' a (len - 1))

main = do
    print (chain 1 1)
    print (chain 1 2)
    print (chain 10 13)
    print (chain 1 (-3))
