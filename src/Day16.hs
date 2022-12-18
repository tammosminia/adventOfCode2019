module Day16 where

import qualified Data.Map as Map
import qualified Data.List as List
import Debug.Trace

type Digit = Integer

input1 = 59781998462438675006185496762485925436970503472751174459080326994618036736403094024111488348676644802419244196591075975610084280308059415695059918368911890852851760032000543205724091764633390765212561307082338287866715489545069566330303873914343745198297391838950197434577938472242535458546669655890258618400619467693925185601880453581947475741536786956920286681271937042394272034410161080365044440682830248774547018223347551308590698989219880430394446893636437913072055636558787182933357009123440661477321673973877875974028654688639313502382365854245311641198762520478010015968789202270746880399268251176490599427469385384364675153461448007234636949

pattern :: Int -> [Integer]
pattern i = tail $ concatMap (\x -> take i $ repeat x) $ cycle [0, 1, 0, -1]

lastDigit :: Integer -> Digit
lastDigit x = (abs x) `mod` 10

phase :: [Digit] -> [Digit]
phase is = map calcLine patterns
  where
    patterns = map pattern [1..(length is)]
    calcLine p = lastDigit $ sum $ map calcCel $ zip is p
    calcCel (x1, x2) = x1 * x2

phases :: Integer -> [Digit] -> [Digit]
phases 0 i = i
phases x i = phases (x - 1) $ phase i

phasesI :: Integer -> Integer -> Integer
phasesI times input = toInt $ phases times $ toDigits input

toDigits :: Integer -> [Digit]
toDigits 0 = []
toDigits i = (toDigits (div i 10)) ++ [mod i 10]


toInt :: [Digit] -> Integer
toInt x = toIntR $ reverse x
  where
    toIntR [] = 0
    toIntR (h : t) = toIntR t * 10 + h

answer1 = toInt $ take 8 $ toDigits $ phasesI 100 input1

