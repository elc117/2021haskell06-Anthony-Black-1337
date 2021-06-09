--Pratica 06 
--Nome: Anthony Carlos Da Silva 

--Q1

ends :: [Int] -> [Int]
ends lst = (head lst):[(last lst)]

--Q2

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = ( x*2) : deduzame xs

--Q3

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs 
  else deduzame2 xs

--Q4

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n,n*n) : geraTabela (n-1)

--Q5

contido :: Char -> String -> Bool 
contido c ""= False 
contido c (x:xs) = if c == x then True else contido c xs 

--Q6
translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate lista = (fst(head lista)+2, snd (head lista)+2) :translate (tail lista)

--Q7

countLongs :: [String] -> Int
countLongs [] = 0
countLongs lista = if length (head lista) > 5 then 1+ countLongs (tail lista) else countLongs(tail lista)

--Q8
onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs list = if length (head list) > 5 then (head list) : onlyLongs (tail list) else onlyLongs(tail list)