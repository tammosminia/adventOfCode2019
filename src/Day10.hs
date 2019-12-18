module Day10 where

import Data.List

input1 = ["#.#.###.#.#....#..##.#....",".....#..#..#..#.#..#.....#",".##.##.##.##.##..#...#...#","#.#...#.#####...###.#.#.#.",".#####.###.#.#.####.#####.","#.#.#.##.#.##...####.#.##.","##....###..#.#..#..#..###.","..##....#.#...##.#.#...###","#.....#.#######..##.##.#..","#.###.#..###.#.#..##.....#","##.#.#.##.#......#####..##","#..##.#.##..###.##.###..##","#..#.###...#.#...#..#.##.#",".#..#.#....###.#.#..##.#.#","#.##.#####..###...#.###.##","#...##..#..##.##.#.##..###","#.#.###.###.....####.##..#","######....#.##....###.#..#","..##.#.####.....###..##.#.","#..#..#...#.####..######..","#####.##...#.#....#....#.#",".#####.##.#.#####..##.#...","#..##..##.#.##.##.####..##",".##..####..#..####.#######","#.#..#.##.#.######....##..",".#.##.##.####......#.##.##"]
example1 = [".#..#",".....","#####","....#","...##"]

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

blocks :: Position -> Position -> Position -> Bool
blocks from middle to = blocks0 (subtractPos middle from) (subtractPos to from)
  where 
    blocks0 (x1, y1) (x2, y2) = (isExactMultiple x1 x2) && (isExactMultiple y1 y2) && isSameMultiple (x1, y1) (x2, y2)
    isExactMultiple x1 x2
      | x1 == x2 = True
      | x1 == 0 = False
      | x2 == 0 = False
      | otherwise = (mod x2 x1) == 0
    isSameMultiple (x1, y1) (x2, y2)
      | x1 == 0 && x2 == 0 = True
      | y1 == 0 && y2 == 0 = True
      | x1 == 0 || y1 == 0 = False
      | otherwise = (div x2 x1) == (div y2 y1)

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
answer1 = bestAsteroid input1
