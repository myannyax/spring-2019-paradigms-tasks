module Basics where
import Prelude hiding (head, tail, take, drop, filter, foldl, concat, (++))

-- Цель первой части домашнего задания -- познакомить вас с основами синтаксиса Хаскеля
-- В этом задании запрещается использовать какие-либо функции из стандартной библиотеки,
-- кроме конструкторов списков и операторов сравнения из `Ord`.
-- Также запрещается использовать list comprehension.
-- Однако разрешается использовать функции, реализованные самостоятельно.

-- 1. head' возвращает первый элемент непустого списка
head' :: [a] -> a
head' (x:_) = x

-- 2. tail' возвращает список без первого элемента, для пустого - пустой
tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

-- 3. take' возвращает первые n >= 0 элементов исходного списка
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : (take' (n - 1) xs)

-- 4. drop' возвращает список без первых n >= 0 элементов; если n больше длины
-- списка, то пустой список.
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 a = a
drop' n (_:xs) = drop' (n - 1) xs

-- 5. filter' возвращает список из элементов, для которых f возвращает True
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x
                   then x:(filter' f xs)
                   else (filter' f xs)

-- 6. foldl'' последовательно применяет функцию f к элементу списка l и значению,
-- полученному на предыдущем шаге, начальное значение
-- foldl'' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)
-- foldl'' (*) 4 [] == 4
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ z [] = z
foldl'' f z (x:xs) = foldl'' f (f z x) xs


-- 7. concat' принимает на вход два списка и возвращает их конкатенацию
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
concat' [] bs = bs
concat' (a:as) bs = a:(concat' as bs)


-- 8. quickSort' возвращает его отсортированный список
-- quickSort' должен быть реализован через алгоритм QuickSort
-- (выбор pivot может быть любым)
quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' a = let
               pivot = head' a
               gt = (> pivot)
               lt = (< pivot)
               eq = (== pivot)
               in concat' (quickSort' (filter' lt a)) (concat' (filter' eq a) (quickSort' (filter' gt a)))