import Prelude hiding (foldl, foldr, minimum, maximum, sum, product, length, and, or, any, all, concat, reverse)
{- 
Фолдове: foldl/foldr, foldl1/foldr1

Идеята е - тръргаме от списък и искаме да го сведем до "една стойност".
Т.е. искаме да "съберем" (collect) или "редуцираме" (reduce), или "сгънем" (fold) входния списък.

Примери: сбор, произведение, най-голям и най-малък елемент, всички, някой ...
-}

-- Примерни рекурсивни дефиниции на foldl и foldr
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)


{-
Задача 1. Използвайте fold(l/r), за да дефинирате следните функции:
а). sum xs, която връща сбора на елементите на xs.
б). product xs, която връща произведението на елементите на xs.
в). length xs, която връща броя на елементите на xs.
г). any p xs, която връща дали предикатът p e верен за поне един от елементите на xs.
д). all p xs, която връща дали предикатът p e верен за всеки от елементите на xs.
е). minimum xs, която връща най-малкия елемент на xs.
ж). maximum xs, която връща най-големия елемент на xs.
з). concat xss, която приема списък от списъци xss и ги конкатенира в един общ списък.

Примери:
    sum [1..10] -> 55
    product [1..10] -> 3628800
    length [1..10] -> 10
    any even [1..10] -> True
    all even [1..10] -> False
    minimum [1..10] -> 1
    maximum [1..10] -> 10
    concat [[1], [2], [3]] -> [1, 2, 3]
-}
sum :: Num a => [a] -> a
sum xs = foldl (+) 0 xs

product :: Num a => [a] -> a
product xs = foldl (*) 1 xs

length :: Num b => [a] -> b
length xs = foldl (\acc x -> acc + 1) 0 xs

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldl (\acc x -> if (p x) then True else acc) False xs

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldl (\acc x -> if (not (p x)) then False else acc) True xs

minimum :: (Num a, Ord a) => [a] -> a
minimum xs = foldl min 1000000 xs

maximum :: (Num a, Ord a) => [a] -> a
maximum xs = foldl max (-100000) xs

concat :: [[a]] -> [a]
concat xs = foldl (++) [] xs


{-
Задача 2. Използвайте fold(l/r), за да дефинирате функцията reverse xs, 
която приема списък xs и обръща елементите му.

Примери: 
    reverse [1, 2, 3] -> [3, 2, 1]
-}
reverse :: [a] -> [a]
reverse xs = foldr (\x acc -> acc ++ [x]) [] xs


{-
Задача 3. Дефинирайте функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))

Пример: 
    compose [(+1), (2*)] 7 -> (2 * 7) + 1 = 15
-}
compose :: [(a -> a)] -> (a -> a)
compose fs = foldl (\acc x -> x acc) (\x -> x) fs


{-
Задача 4*. Дефинирайте функцията combinations xss, която приема списък от списъци xss 
и връща списък с всички възможни комбинации, съдържащи по един елемент от всеки от
подсписъците на xss.

Пример:
    combinations [[1, 2, 3], [4], [5, 6]] -> [[1,4,5], [1,4,6], [2,4,5], [2,4,6], [3,4,5], [3,4,6]]

    -- Генератор на всички RGB цветове (има само 16 милиона елемента, така че НЕ се опитвайте да го изпринтите :) ...
    rgbColors = combinations [reds, blues, greens] where
        [reds, blues, greens] = replicate 3 [0..255]

    -- Генератор на менюта
    предястия   = ["Салата", "Супа"]
    основни     = ["Бургер", "Пица", "Паста", "Качамак"]
    десерти     = ["Крем карамел", "Палачинки", "Сладолед"]
    менюта      = combinations [предястия, основни, десерти]
-}
combinations :: [[a]] -> [[a]]
combinations xss = undefined


{-
Задача 5*. Братята Алън и Боб искат да си поделят комплект от n подаръка. 
Всеки от подаръците трябва да се даде или на Алън, или на Боб, като нито 
един от подаръците не може да се раздели на две. Всеки подарък има стойност 
цяло положително число. Нека a и b означават сумата на подаръците, получени 
съответно от Алън и от Боб - целта е да се минимизира абсолютната стойност 
на разликата a - b.

Да се дефинира функцията splitEven gifts, която получава списък gifts със 
стойностите на всеки от подаръците и връща точкова двойка от числата a и b.

Пример:
    splitEven [3, 2, 3, 2, 2, 77, 89, 23, 90, 11] -> (136, 166)
-}
splitEven :: (Integral n) => [n] -> (n, n)
splitEven gifts = undefined


-- примери от условията
main :: IO()
main = do
    -- Задача 1.
    print $ sum [1..10]
    print $ product [1..10]
    print $ length [1..10]
    print $ any even [1..10]
    print $ all even [1..10]
    print $ minimum [1..10]
    print $ maximum [1..10]
    print $ concat [[1], [2], [3]]
    
    -- Задача 2.
    print $ reverse [1, 2, 3]

    -- Задача 3.
    print $ compose [(+1), (2*)] 7

    -- Задача 4.
    print $ combinations [[1, 2, 3], [4], [5, 6]]

    -- Задача 5.
    print $ splitEven [3, 2, 3, 2, 2, 77, 89, 23, 90, 11]