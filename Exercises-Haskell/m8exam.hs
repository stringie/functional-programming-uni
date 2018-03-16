lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

firstDigits :: Integer -> Integer
firstDigits n = n `div` 10

numDigits :: Integer -> Integer
numDigits n =
    if n < 10 
    then 1
    else 1 + numDigits (firstDigits n)

{-
Задача. 1. Да се напише предикат isAscending, който връща истина, ако цифрите на
дадено естествено число са в нарастващ ред от първата към последната.

    isAscending 5    -> True
    isAscending 121  -> False
    isAscending 122  -> True
    isAscending 123  -> True
-}
isAscending :: Integer -> Bool
isAscending n
    | n < 10 = True
    | otherwise = lastDigit allButLast <= lastDigit n && isAscending (firstDigits n)
        where allButLast = firstDigits n

{-
Задача. 2. Да се напише функция countOccurences, намираща броя на срещанията на дадена
цифра d в записа на число n.

    countOccurences 5 5      -> 1
    countOccurences 5 25525  -> 3
    countOccurences 5 12346  -> 0
-}
countOccurences :: Integer -> Integer -> Integer
countOccurences d n
    | n == 0 = 0
    | lastDigit n == d = 1 + countOccurences d (firstDigits n)
    | otherwise = countOccurences d (firstDigits n)

{-
Задача. 3. Напишете функция, която за дадено неотрицателно цяло число проверява
дали на всяка четна позиция в десетичния запис на числото стои нечетна цифра. 
Нека старшата (най-лявата) цифра на числото има позиция 0.

    evenPosOddDigits 5    -> True
    evenPosOddDigits 6    -> False
    evenPosOddDigits 123  -> True
    evenPosOddDigits 122  -> False
-}
evenPosOddDigits :: Integer -> Bool
evenPosOddDigits n = helper 0 n
    where helper pos num
            | pos == n = True
            | even ((numDigits n) - 1 - pos) = odd (lastDigit n) && helper (pos + 1) (firstDigits n)
            | otherwise = helper (pos + 1) (firstDigits n)

{-
Задача 4. Да се дефинира функция която взима числото, което се образува
от последните n цифри на числото m.
-}
takeNFromEnd :: Integer -> Integer -> Integer
takeNFromEnd 0 m = 0
takeNFromEnd n m = lastDigit m + 10 * (takeNFromEnd (n-1) (firstDigits m))


{-
Задача. 5. Да се дефинира предикат isAutomorphic, който приема число n и
проверява дали n^2 завършва с цифрите на n.

isAutomorphic 2
isAutomorphic 5
isAutomorphic 25
isAutomorphic 26
-}

isAutomorphic :: Integer -> Bool
isAutomorphic n = n == takeNFromEnd (numDigits n) (n^2)

{-
Задача. 6*. Да се дефинира функция, която намира броя на срещанията
на многоцифрено число в записа на число n. (Като в задача 2, но не само
с едноцифрени числа.)
-}
countOccurences' :: Integer -> Integer -> Integer
countOccurences' d n
    | numDigits d > numDigits n = 0
    | takeNFromEnd (numDigits d) n == d = 1 + countOccurences' d (firstDigits n)
    | otherwise = countOccurences' d (firstDigits n) 


main = do
    print $ takeNFromEnd 2 12345
    print $ "space"

    print $ isAscending 5
    print $ isAscending 121
    print $ isAscending 122
    print $ isAscending 123
    
    print $ countOccurences 5 5
    print $ countOccurences 5 25525
    print $ countOccurences 5 12346
    
    print $ evenPosOddDigits 5
    print $ evenPosOddDigits 6
    print $ evenPosOddDigits 123
    print $ evenPosOddDigits 122
    
    print $ isAutomorphic 2
    print $ isAutomorphic 5
    print $ isAutomorphic 25
    print $ isAutomorphic 26

    
    
