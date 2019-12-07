module Day4 where

range = [353096..843212]

type Digit = Int

toDigits :: Int -> [Digit]
toDigits 0 = []
toDigits i = (toDigits (div i 10)) ++ [mod i 10]

sameAdjacent :: Eq a => [a] -> Bool
sameAdjacent (x : y : _) 
  | x == y = True
sameAdjacent (h : t) = sameAdjacent t
sameAdjacent _ = False

neverDecrease :: Ord a => [a] -> Bool
neverDecrease [] = True
neverDecrease [_] = True
neverDecrease (x1 : x2 : xs)
  | x1 > x2 = False
  | otherwise = neverDecrease (x2 : xs)
  
checkNumber :: Int -> Bool
checkNumber x = sameAdjacent d && neverDecrease d
                where d = toDigits x 

valid1 = filter checkNumber range
answer1 = length valid1

split :: Eq a => [a] -> [[a]]
split [] = []
split xs@(x : _) = (takeWhile (==x) xs) : split (dropWhile (==x) xs)

doubleAdjacent :: Eq a => [a] -> Bool
doubleAdjacent l = any (\x -> (length x) == 2) (split l)

checkNumber2 :: Int -> Bool
checkNumber2 x = sameAdjacent d && neverDecrease d && doubleAdjacent d
                where d = toDigits x 

valid2 = filter checkNumber2 range
answer2 = length valid2
