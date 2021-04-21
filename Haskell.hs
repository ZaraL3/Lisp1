-- 3. Определите функцию, которая разделит исходный список из целых чисел на два списка: список положительных чисел и список отрицательных чисел
print("TASK 3")
split list = (filter (> 0) list,filter (< 0) list)
print("TEST CASES")
print (split [1,2,3,4])
--([1,2,3,4],[])
print (split [-1,-2,-3,-4])
--([],[-1,-2,-3,-4])
print (split [1,-1,2,-2,3,-3,4,-4])
--([1,2,3,4],[-1,-2,-3,-4])

-- 14. Определите функцию, вычисляющую глубину списка (самой глубокой ветви).
-- В haskell невозможно создать вложенные списки без создания собственных структур, поэтому будет использоваться стандартный тип Data.Tree
print("TASK 14")
import Data.Tree
import Data.List -- maximum
:{
getDepth :: Tree a -> Int
getDepth (Node _ []) = 1
getDepth (Node _ xs) = 1 + maximum (map getDepth xs)
:}
print("TEST CASES")
print (getDepth (Node 1 [Node 2 [], Node 3 []]))
--2
print (getDepth (Node 1 [Node 2 [Node 4 []], Node 3 []]))
--3
print (getDepth (Node 1 [Node 2 [Node 4 []], Node 3 [Node 5[]]]))
--3

-- 15 Определите функцию (ПЕРВЫЙ-СОВПАДАЮЩИЙ х у), 
--которая возвращает первый элемент, входящий в оба списка х и у, в противном случае NIL.
print("TASK 15")
first_coincident [] y = []
first_coincident (x:xs) y = if (memory1 x y)
                            then [x]
                            else (first_coincident xs y)
                         
print("TEST CASES")
print (first_coincident [3, 4, 0, 5, 7] [2, 1, 5, 7, 5])
[5]
print (first_coincident [3, 4, 0, -1] [2, 1, 5, 7, 5])
[]
print (first_coincident [4] [])
[]


 
-- 5 Определите функция, упаковывающую последовательные дубликаты списка
--в подсписки вида (M N), где N - элемент списка, M - количество повторений. 
--Например, [’a’, ’a’, ’a’, ’a’, ’b’, ’c’, ’c’, ’a’, ’a’, ’d’, ’e’,’e’, ’e’, ’e’] 
--должен быть переведен в [(4, ’a’), (1, ’b’), (2, ’c’),(2, ’a’), (1, ’d’), (4, ’e’)].

get_chunk_len (f:[]) = 1 
get_chunk_len (f:s:r) = 
   if (s /= f) 
   then 1 
   else 1 + get_chunk_len (s:r) 
     remove_chunk (f:[]) = [] 
     remove_chunk (f:s:r) = 
       if (s /= f) 
       then s:r 
       else remove_chunk (s:r) 
          compress [] = [] 
          compress (lst) = [(get_chunk_len lst, head lst)] ++ compress (remove_chunk lst) 