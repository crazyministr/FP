-- Малашенков Антон, гр. А3401

module Palindrome where

-- Число назовем палиндромом, если цифры его десятичного представления одни и те же 
-- при чтении числа слева направо и справа налево. 
-- Например, числа 5, 1771 и 222 - палиндромы, а 17, 500 и 10002 - нет. 
-- Написать функцию palindrom :: Integer -> Bool, которая проверяет, является ли число палиндромом.

-- Функция reverseNumber предназначена для "разворота" целого числа
-- В качестве параметра принимает целое число, которое нужно "развернуть"
-- На выходе отдаёт "развёрнутое" число
reverseNumber :: Integer -> Integer
-- Конвертируем число в стоку (функция show)
-- "Разворачиваем число" (фукнция reverse)
-- Конвертируем строку в число (функция read)
reverseNumber number = read . reverse . show $ number

palindrome :: Integer -> Bool
palindrome number | number < 0 = False
                  | otherwise  = number == reverseNumber number

test = (palindrome 5 == True,
       palindrome 1771 == True,
       palindrome 222 == True,

       palindrome 17 == False,
       palindrome 500 == False,
       palindrome 10002 == False,

       palindrome (-13) == False,
       palindrome 13 == False)

main = do
    print $ test
