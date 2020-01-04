module Day12 where

import Text.Regex
import Data.List

data Pos = Pos { x :: Int, y :: Int, z :: Int } deriving (Eq)
type Velocity = Pos
data Simulation = Simulation { ps :: [Pos], vs :: [Velocity]} deriving (Show, Eq)

instance Read Pos where
  readsPrec _ s = [(Pos { x = paramInt "x", y = paramInt "y", z = paramInt "z"}, "")]
    where
      paramInt param = read (paramStr param) :: Int
      paramStr :: String -> String
      paramStr param = case (matchRegex (mkRegex (param ++ "=([0-9\\-]+)")) s) of
        Just [v] -> v
        _ -> error "cannot parse"

instance Show Pos where
  show Pos {x = x, y = y, z = z} = "<x=" ++ (show x) ++ ", y=" ++ (show y) ++ ", z=" ++ (show z) ++">"

pos0 = Pos {x=0, y=0, z=0}

input1s = ["<x=1, y=2, z=-9>", "<x=-1, y=-9, z=-4>", "<x=17, y=6, z=8>", "<x=12, y=4, z=2>"]
example1s = ["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"]
example2s = ["<x=-8, y=-10, z=0>", "<x=5, y=5, z=10>", "<x=2, y=-7, z=3>", "<x=9, y=-8, z=-3>"]

initSimulation :: [String] -> Simulation
initSimulation ss = Simulation { ps=ps, vs=vs}
  where
    ps = map (\x -> read x :: Pos) ss
    vs = map (\x -> pos0) ps

allDim :: (Int -> Int -> Int) -> Pos -> Pos -> Pos
allDim f Pos {x=x1, y=y1, z=z1} Pos {x=x2, y=y2, z=z2} = Pos {x=f x1 x2, y=f y1 y2, z=f z1 z2}

add :: Pos -> Pos -> Pos
add = allDim (+)

addLists :: [Pos] -> [Pos] -> [Pos]
addLists ps1 ps2 = map (\(p1, p2) -> add p1 p2) $ zip ps1 ps2

gravity :: Pos -> Pos -> Velocity
gravity = allDim g1
  where
    g1 x1 x2
      | x1 < x2 = 1
      | x1 > x2 = (-1)
      | otherwise = 0

allGravity :: [Pos] -> [Velocity]
allGravity moons = map gMoon moons
  where
    gMoon moon = foldl add pos0 vChanges
      where vChanges = map (gravity moon) (moons \\ [moon])

step :: Simulation -> Simulation
step Simulation {ps=ps, vs=vs} = Simulation {ps=newPs, vs=newVs}
  where
    newVs = addLists vs (allGravity ps)
    newPs = addLists ps newVs

steps :: Int -> Simulation -> Simulation
steps 0 s = s
steps i s = steps (i - 1) $ step s

energy :: Pos -> Int
energy Pos {x=x, y=y, z=z} = (abs x) + (abs y) + (abs z)

energyForSim :: Simulation -> Int
energyForSim Simulation {ps=ps, vs=vs} = sum $ map (\(p, v) -> p * v) $ zip (map energy ps) (map energy vs)

answer1 = energyForSim (steps 1000 (initSimulation input1s))

mapSim :: (Pos -> Pos) -> Simulation -> Simulation
mapSim f Simulation {ps=ps, vs=vs} = Simulation {ps=map f ps, vs=map f vs}

repeatIn :: Simulation -> Int
repeatIn start = repeat (step start) 1
  where
    repeat s i
      | s == start = i
      | otherwise = repeat (step s) (i + 1)

repeatSmart :: Simulation -> Int
repeatSmart s = lcm rz $ lcm rx ry
  where
    rf f = repeatIn $ mapSim f s
    rx = rf (\p -> Pos {x=x p, y=0, z=0})
    ry = rf (\p -> Pos {x=0, y=y p, z=0})
    rz = rf (\p -> Pos {x=0, y=0, z=z p})

answer2 = repeatSmart $ initSimulation input1s

