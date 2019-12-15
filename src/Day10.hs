module Day10 where

import Data.List

input1 = ["#.#.###.#.#....#..##.#....",".....#..#..#..#.#..#.....#",".##.##.##.##.##..#...#...#","#.#...#.#####...###.#.#.#.",".#####.###.#.#.####.#####.","#.#.#.##.#.##...####.#.##.","##....###..#.#..#..#..###.","..##....#.#...##.#.#...###","#.....#.#######..##.##.#..","#.###.#..###.#.#..##.....#","##.#.#.##.#......#####..##","#..##.#.##..###.##.###..##","#..#.###...#.#...#..#.##.#",".#..#.#....###.#.#..##.#.#","#.##.#####..###...#.###.##","#...##..#..##.##.#.##..###","#.#.###.###.....####.##..#","######....#.##....###.#..#","..##.#.####.....###..##.#.","#..#..#...#.####..######..","#####.##...#.#....#....#.#",".#####.##.#.#####..##.#...","#..##..##.#.##.##.####..##",".##..####..#..####.#######","#.#..#.##.#.######....##..",".#.##.##.####......#.##.##"]

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
blocks from to middle = blocks0 (subtractPos middle from) (subtractPos to from)
  where blocks0 (x1, y1) (x2, y2) = (mod x2 x1) == 0 && (mod y2 y1) == 0 && (div x2 x1) == (div y2 y1)

visibles :: Position -> [Position] -> [Position]
visibles from all = filter (visible all from) all

visible :: [Position] -> Position -> Position -> Bool
visible all from to = any (blocks from to) (all \\ [from, to])

positions1 = parseInput input1
visibles1 = map (\x -> (x, visibles x positions1)) positions1
answer1 = head $ sort $ map (\(s, vs) -> (length vs, s)) visibles1