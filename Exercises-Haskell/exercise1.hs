{- ПРИМЕРИ -}

-- Пример 1. Обиколка на окръжност с радиус r.
perimeter :: Double -> Double
perimeter r = 
    if r < 0 
        then error "r >= 0" 
        else 2 * pi * r


-- Пример 2. factorial n, която пресмята факториела на целочисленият аргумент n.
factorial :: Integer -> Integer
factorial n 
    | n < 0     = error "n >= 0"            -- съобщение за грешка при неправилни входни данни 
    | n == 0    = 1                         -- базов случай
    | otherwise = n * factorial (n - 1)     -- общ случай


-- Пример 3. Провека дали цялото число k е делител на числото n:
divides :: Integer -> Integer -> Bool
k `divides` n = n `mod` k == 0


{- ЗАДАЧИ -}

{-
Задача 1. Напишете функцията max3 x y z, която връща най-голямото от числатa x, y и z.
За целта използвайте функцията max x y, която връща по-голямото от двете числа.
-}
max3 :: Int -> Int -> Int -> Int
max3 x y z 
        | max x y == x && max x z == x = x
        | max x y == y && max y z == y = y
        | max z y == z && max x z == z = z


{-
Задача 2. Редица на Фибоначи: напищете функцията fib n, която връща n-тото число от редицата на
Фибоначи, дефинирана като:

    fib(0) = 0
    fib(1) = 1
    fib(n) = fib(n - 1) + fib(n - 2)

    Пример:
        fib 3 = 2
        fib 8 = 21
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


{-
Задача 3. Прости числа: дефинирайте функцията isPrime n, която приема цялото число n и проверява 
дали то е просто.
-}
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = and $ map (\x -> n `mod` x /= 0) [2..(n - 1)]


{-
Задача 4. Да се дефинира функцията isNarcissistic n, която приема като аргумент цялото положително
число n и връща дали то е нарцистично. Нарцистични се наричат числата, които са равни на сбора на 
цифрите си (в десетична бройна система), всяка повдигната на степен броя на цифрите на числото.

Пример за такова число е 153, тъй като 1 ^ 3 + 5 ^ 3 + 3 ^ 3 = 1 + 125 + 27 = 153. 
-}
intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = n `mod` 10 : intToList (n `div` 10)

isNarcissistic :: Integer -> Bool
isNarcissistic n = (==) n $ sum $ map (^(length listNums)) listNums
        where listNums = intToList n


{-
Задача 5. Да се дефинира функцията isPerfectNumber n, която приема целочисления аргумент n и
връща дали той е перфектно число. Перфектно число се нарича всяко чяло число, равно на сбора на
собствeните си делители.

Примери: 
    6 = 1 + 2 + 3
    28 = 1 + 2 + 4 + 7 + 14
-}
isPerfectNumber :: Integer -> Bool
isPerfectNumber n = (==) n $ sum $ filter (`divides` n) [1..(n - 1)]


{-
Задача 6. Напишете оператор n ## k, който взима n > 0 и k >= 0 и връща сумата от всяка цифра
на n повдигната на степен k.

    Пример: 
        12 ## 2 = 1 ^ 2 + 2 ^ 2 = 1 + 4   = 5
        17 ## 3 = 1 ^ 3 + 7 ^ 3 = 1 + 343 = 344
-}
(##) :: Integer -> Integer -> Integer   -- Когато пишем типа на оператора, ограждаме името му със скоби.
n ## k = sum $ map (^k) listNums
        where listNums = intToList n


-- main функция с примерни извиквания
main :: IO()
main = do 
    -- Примери:
    print $ perimeter 10
    print $ factorial 5
    print $ 3 `divides` 7
    print $ 3 `divides` 9

    -- Задача 1.
    print $ max3 3 1 7
    print $ max3 3 12 7
    print $ max3 50 1 7

    -- Задача 2.
    print $ fib 3
    print $ fib 8

    -- Задача 3.
    print $ isPrime 2
    print $ isPrime 4
    print $ isPrime 97

    -- Задача 4.
    print $ isNarcissistic 153
    print $ isNarcissistic 28

    -- Задача 5.
    print $ isPerfectNumber 6
    print $ isPerfectNumber 28
    print $ isPerfectNumber 42

    -- Задача 6.
    print $ 12 ## 2
    print $ 17 ## 3