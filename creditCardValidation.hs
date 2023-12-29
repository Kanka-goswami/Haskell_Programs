-- Validating Credit Card Numbers
-- Solutions: https://github.com/xyc/cis194/blob/master/cis194-spring13/hw1/hw1.hs, accessed on 26 Dec. 2023
{-
	n this section, you will implement the validation algorithm for
credit cards. It follows these steps:
• Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is dou-
bled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].
• Add the digits of the doubled values and the undoubled dig-
its from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18.
alculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.
-}


-- Exercise 1
{-
    We need to first find the digits of a number.
    Define the functions
    
    toDigits :: Integer -> [Integer]
    
    toDigitsRev :: Integer -> [Integer]
    
    toDigits should convert positive Integers to a list of digits. (For 0 or negative inputs, toDigits should return the empty list.) toDigitsRev should do the same, but with the digits reversed.
-}

toDigits :: Integer -> [Integer]
toDigits x
    | x<0 =[]
    | x==0 = []
    | x>0 = toDigits (x `div` 10) ++ [x `mod` 10]
    

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x<0 = []
    | x==0 = []
    | x>0 = x `mod` 10 : toDigitsRev (x `div` 10)
    
-- Exercise 2
{-
    Once we have the digits in the proper order, we need to double every other one. Define a function
    
    doubleEveryOther :: [Integer] -> [Integer]
    
    Remember that doubleEveryOther should double every other number beginning from the right, that is, the second-to-last, fourth-to-last, . . . numbers are doubled.
    
    https://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list, accessed on 26 Dec. 2023
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs) = x : (2*head xs) : doubleEveryOther (tail xs)

-- Exercise 3
{-
    Define the function
    
    sumDigits :: [Integer] -> Integer

    to calculate the sum of all digits.
    Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}
sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits d = sum (map (sumDigit) d)

-- Exercise 4
{-
    Define the function
    
    validate :: Integer -> Bool
    
    that indicates whether an Integer could be a valid credit card number. This will use all functions defined in the previous exercises.
    Example: validate 4012888888881881 = True
    Example: validate 4012888888881882 = False
-}
validate :: Integer -> Bool
validate v = sumDigits (doubleEveryOther (toDigitsRev v)) `mod` 10 == 0