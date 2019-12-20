module Day10 where

import Data.List

input1 = ["#.#.###.#.#....#..##.#....",".....#..#..#..#.#..#.....#",".##.##.##.##.##..#...#...#","#.#...#.#####...###.#.#.#.",".#####.###.#.#.####.#####.","#.#.#.##.#.##...####.#.##.","##....###..#.#..#..#..###.","..##....#.#...##.#.#...###","#.....#.#######..##.##.#..","#.###.#..###.#.#..##.....#","##.#.#.##.#......#####..##","#..##.#.##..###.##.###..##","#..#.###...#.#...#..#.##.#",".#..#.#....###.#.#..##.#.#","#.##.#####..###...#.###.##","#...##..#..##.##.#.##..###","#.#.###.###.....####.##..#","######....#.##....###.#..#","..##.#.####.....###..##.#.","#..#..#...#.####..######..","#####.##...#.#....#....#.#",".#####.##.#.#####..##.#...","#..##..##.#.##.##.####..##",".##..####..#..####.#######","#.#..#.##.#.######....##..",".#.##.##.####......#.##.##"]
example1 = [".#..#",".....","#####","....#","...##"]
example2 = ["......#.#.","#..#.#....","..#######.",".#.#.###..",".#..#.....","..#....#.#","#..#....#.",".##.#..###","##...#..#.",".#....####"]
example3 = ["#.#...#.#.",".###....#.",".#....#...","##.#.#.#.#","....#.#.#.",".##..###.#","..#...##..","..##....##","......#...",".####.###."]
example4 = [".#..#..###","####.###.#","....###.#.","..###.##.#","##.##.#.#.","....###..#","..#.#..#.#","#..#.#.###",".##...##.#",".....#.#.."]
example5 = [".#..##.###...#######","##.############..##.",".#.######.########.#",".###.#######.####.#.","#####.##.#.##.###.##","..#####..#.#########","####################","#.####....###.#.#.##","##.#################","#####.##.###..####..","..######..##.#######","####.##.####...##..#",".#####..#.######.###","##...#.##########...","#.##########.#######",".####.#.###.###.#.##","....##.##.###..#####",".#.#.###########.###","#.#.#.#####.####.###","###.##.####.##.#..##"]

example21 = [".#....#####...#..","##...##.#####..##","##...#...#.#####.","..#.........###..","..#.#.....#....##"]

type Position = (Int, Int) -- (x,y)  (0,0) is top left

parseInput :: [String] -> [Position]
parseInput i = parse 0 i
  where
    parse _ [] = []
    parse y (h : t) = (parseLine 0 y h) ++ parse (y + 1) t
    parseLine _ _ [] = []
    parseLine x y ('#' : t) = (x, y) : parseLine (x + 1) y t
    parseLine x y (_ : t) = parseLine (x + 1) y t

subtractPos :: Position -> Position -> Position
subtractPos (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

addPos :: Position -> Position -> Position
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

blocks :: Position -> Position -> Position -> Bool
blocks from middle to = blocks0 (subtractPos middle from) (subtractPos to from)
  where 
    blocks0 m t = (sameDirection m t) && (smaller m t)
    sameDirection m t = (directionOf m) == (directionOf t)
    smaller (x1, y1) (x2, y2) = ((abs x1) <= (abs x2)) && ((abs y1) <= (abs y2))

directionOf :: Position -> Position
directionOf (x, y)
  | d == 0 = (x, y)
  | otherwise = (div x d, div y d)
  where d = gcd x y

visibles :: Position -> [Position] -> [Position]
visibles from all = filter (visible all from) $ all \\ [from]

visible :: [Position] -> Position -> Position -> Bool
visible all from to = not $ any (\x -> blocks from x to) (all \\ [from, to])

bestAsteroid :: [String] -> (Int, Position)
bestAsteroid i = last $ sort $ map (\(s, vs) -> (length vs, s)) $ allVisible i

allVisible :: [String] -> [(Position, [Position])]
allVisible i = map (\x -> (x, visibles x positions)) positions
  where positions = parseInput i

positions1 = parseInput input1
visibles1 = map (\x -> (x, visibles x positions1)) positions1
(answer1, station1) = bestAsteroid input1

relativeToBase :: Position -> [Position] -> [Position]
relativeToBase base = map (\x -> subtractPos x base)

sortClockwise :: [Position] -> [Position]
sortClockwise = sortBy (\x1 x2 -> compare (angle x1) (angle x2))

angle :: Position -> Float -- relativePosition -> angle [0..2pi>
angle (x, y)
  | x >= 0 && y < 0 = atan (fromIntegral x / fromIntegral (-y))
  | x > 0 && y >= 0 = pi / 2 + atan (fromIntegral y / fromIntegral x)
  | x <= 0 && y > 0 = pi + atan (fromIntegral (-x) / fromIntegral y)
  | x < 0 && y <= 0 = pi * 3 / 2 + atan (fromIntegral (-y) / fromIntegral (-x))

vaporize :: Position -> [Position] -> [Position]
vaporize base l = map (addPos base) $ vapR targets
  where
    targets = relativeToBase base $ l \\ [base]
    vapR [] = []
    vapR l = let thisRound = sortClockwise $ visibles (0,0) l
      in thisRound ++ vapR (l \\ thisRound)


vaporize2 = vaporize station1 positions1
answerpos2 = vaporize2 !! 199
answer2 = let (x,y) = answerpos2 in x*100 + y
