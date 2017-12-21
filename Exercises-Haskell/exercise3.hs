import Data.Char
import Data.List
{-
I. Вектори (кортежи, tuples): има три основни разлики между векторите и масивите.
 1. Масивите се пишат с квадратни [], а векторите с кръгли () скоби.

 2. Всички елементи на масива трявбва да са от един и същ тип, т.е. [1, 3.14, "Kasa bira!"]
    ще ви даде грешка. За сметка на това, всеки един от елементите на даден вектор, може да 
    бъде от различен тип, т.е. (1, 3.14, "Kasa bira!") е напълно валидно.

 3. Веднъж зададен, броят на елементите на един вектор е фиксиран, докато списъците могат
    да растат произволно.


На практика най-често използваните вектори са двойките (pairs). Основните функции върху 
тях са fst и snd, които връщат съответно първия и втория елемент на двойката.

    fst (1, 2) -> 1
    snd (1, 2) -> 2

Едно интересно приложение на векторите е присвояването на няколко променливи наведнъж:
    (a, b, c) = (1, 3.14, "Kasa bira!")

което е (почти) същото като:
    a = 1
    b = 3.14
    c = "Kasa bira!"

Няколко примера за типове на вектори:
 - (Ingeger, Bool) - вектор от цяло число и булева стойност. 
 - (String, Double) - вектор от символен низ и дробно число.
 - (Integer, [String], Double) - вектор от цяло число, масив от символни низове и дробно число.
 - ((String, Bool), [[Integer]]) - вектор от двойка от символен низ и булева стойност, и масив 
   от масиви от цели числа.


II. Ламбда изрази: както в Racket, така и в Haskell всички ф-и са ламбда изрази. 
    
В Haskell ламбда изразите изглеждат така:

> foo :: Int -> Int 
> foo = \x -> 2 * x

което е АБСОЛЮТНО съшото като:

> foo :: Int -> Int 
> foo x = 2 * x

Още примери (Групираните по-долу дефиниции са еквивалиентни)

> bar :: Int -> Int -> Int
> bar =   \x ->  \y -> x + y
> bar = \x y -> x + y
> bar x y = x + y

> baz :: Int -> Int -> Int -> Int
> baz =   \x ->  \y ->  \z -> x + y * z
> baz = \x y z -> x + y * z
> baz = \x -> \y -> \z -> x + y * z


III. Още полезни функции върху списъци: 
 - map/filter: работят, както в Racket, но map приема само едноаргументни функции
 - zip xs ys: приема два списъка и ги обединява в един списък от двоики елемент по
   елемент. 
 - zipWith: zip + map
 - concatMap: map + concat
 - and/or/any/all
 - takeWhile/dropWhile
 - list comprehension:

> ys = [f x | x <- xs, p x] е същото като:
> ys = map f (filter p xs)

Примери:

> map (\x -> x ^ 2) [1, 2, 3, 4]                    => [1,4,9,16]
> filter even [1 .. 10]                             => [2,4,6,8,10]
> zip [1 .. 10] ['a' .. 'z']                        => [(1,'a'),(2,'b'), ... ,(10,'j')]
> zipWith (+) [1, 2, 3] [10, 20, 30]                => [11,22,33]
> concatMap (\(x, y) -> [x, y]) [(1, 2), (3, 4)]    => [1,2,3,4]
> and [True, False]         => False
> or [False, False, True]   => True
> all even [2, 4, 6]        => True
> any odd [2, 3, 4, 6]      => True

> [x ^ 2 | x <- [1, 2, 3, 4]]           => [1,4,9,16]
> [x ^ 3 | x <- [1, 2, 3, 4], even x]   => [8,64]

-}

{-
Задача 1. Да се дефинира функция filterPrimePositions xs която получава списък xs
и връща нов списък съдържащ елементите от xs, чиято позиция в xs e просто число. 

В тази задача индексирането на елементите от xs ще започва от 2!

Примери:
    filterPrimePositions [1, 2, 3] = [1, 2]
    filterPrimePositions [2, 3, 4, 5, 6, 7] = [2, 3, 5, 7]
-}
filterPrimePositions :: [a] -> [a]
filterPrimePositions xs = zipWith (!!) (replicate (length primePos) xs) $ map (subtract 2) primePos
    where isPrime 0 = False
          isPrime 1 = False
          isPrime n = and $ map (\x -> n `mod` x /= 0) [2..(n - 1)] 
          primePos = filter isPrime [2..(length xs)+1]


{-
Задача 2. Брой на повторенията на най-малкото число в списък: Да се дефинира 
функцията countMinimum xs, която приема несортиран списък xs, съставен само от 
положителни цели числа и връща броя на срещанията на най-малкия елемент от списъка. 

Примери:
    countMinimum [1, 2, 1, 1, 5, 3] = 3 
    countMinimum [3, 4, 2] = 1
    countMinimum [] = 0
-}
countMinimum :: [Integer] -> Int
countMinimum xs = length $ filter (==(minimum xs)) xs


{-
Задача 3. Дефинирайте функцията title str, която превръща в главна първата 
буква на всяка дума от символния низ str, а останалите в малки.

Пример: 
    title "the souND aND tHe fuRY" = "The Sound And The Fury"
-}
title :: String -> String
title = unwords . map (\w -> [toUpper $ head w] ++ map toLower (tail w)) . words


{-
Задача 4. Напишете функцията images f g xys, която приема функции f, g :: Double -> Double и
списък от двойки от тип (Double, Double) и връща списъка от тези двойки (x, y), за които
f(x) e равно на g(y).

Пример: 
    images (\x -> x * x) (\x -> x + 2) [(2, 2), (1, 2), (3, 7)] = [(2, 2), (3, 7)]
-}
images :: (Double -> Double) -> (Double -> Double) -> [(Double, Double)] -> [(Double, Double)]
images f g = filter (\p -> f (fst p) == g (snd p))


{-
Задача 5. Напишете функциията isIterator f xs, която проверява дали всеки от елементите на
списъка xs (без първия) се получава чрез прилагането на f върху предишния елемент от списъка.

Пример:
    isIterator (+2) [1, 3, 5] = True
    isIterator (+2) [1, 3, 4] = False
-}
isIterator :: (Eq a) => (a -> a) -> [a] -> Bool
isIterator f xs = and $ zipWith (==) xs (iterate f (head xs))


{-
Задача 6. Панграми: напишете функция isPangram str, която проверява дали символният 
низ str е панграма. Панграма се нарича символен низ, който съдържа всеки символ от 
азбуката.

ПримерИ: 
    isPangram "The quick brown fox jumps over the lazy dog." = True
    isPangram "The quick brown fox jumps over the fence." = False
-}
isPangram :: String -> Bool
isPangram str = str `union` ['a'..'z'] == str

-- main функция с примерни извиквания на функциите от задачите.
main :: IO()
main = do
    -- Задача 1.
    print $ filterPrimePositions [1, 2, 3]
    print $ filterPrimePositions [2, 3, 4, 5, 6, 7]

    -- Задача 2.
    print $ countMinimum [1, 2, 1, 1, 5, 3]
    print $ countMinimum [3, 4, 2]
    print $ countMinimum []

    -- Задача 3.
    print $ title "the souND aND tHe fuRY"

    -- Задача 4.
    print $ images (\x -> x * x) (\x -> x + 2) [(2, 2), (1, 2), (3, 7)]

    -- Задача 5.
    print $ isIterator (+2) [1, 3, 5]
    print $ isIterator (+2) [1, 3, 4]

    -- Задача 6.
    print $ isPangram "The quick brown fox jumps over the lazy dog."
    print $ isPangram "The quick brown fox jumps over the fence."