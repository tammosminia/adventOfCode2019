module Intcode where

import qualified Data.Map as Map

type Program = [Int]
type Position = Int
type Value = Int
type Opcode = Int
type NrInputs = Int
type NrParameters = Int
type PipeInput = Bool
data OutputType = OutputNothing | OutputInProgram | OutputInPipe | JumpPosition | ChangeRelativeBase deriving (Show, Eq)
type JumpOutput = Bool
type Input = Value
type Output = Value
data ParameterType = PtImmediate | PtPosition | PtRelative deriving (Show)
type Instruction = (NrParameters, NrInputs, PipeInput, OutputType, [Input] -> [Output])
data RunningProgram = RunningProgram { position :: Position, program :: Program, inputs :: [Input], relativeBase :: Position} deriving (Show, Eq)


instructions :: Map.Map Opcode Instruction
instructions = Map.fromList [(1, plus), (2, product), (3, pipeIn), (4, pipeOut), (5, jumpTrue), (6, jumpFalse), (7, lessThan), (8, equals), (9, changeRelativeBase), (99, end)]
  where
    plus = (3, 2, False, OutputInProgram, (\[a,b] -> [a + b]))
    product = (3, 2, False, OutputInProgram, (\[a,b] -> [a * b]))
    pipeIn = (1, 1, True, OutputInProgram, id)
    pipeOut = (1, 1, False, OutputInPipe, id)
    jumpTrue = (2, 2, False, JumpPosition, (\[x, p] -> if (x /= 0) then [p] else []))
    jumpFalse = (2, 2, False, JumpPosition, (\[x, p] -> if (x == 0) then [p] else []))
    lessThan = (3, 2, False, OutputInProgram, (\[a, b] -> if (a < b) then [1] else [0]))
    equals = (3, 2, False, OutputInProgram, (\[a, b] -> if (a == b) then [1] else [0]))
    changeRelativeBase = (1, 1, False, ChangeRelativeBase, id)
    end = (0, 0, False, OutputNothing, id)

fullRun :: Program -> [Input] -> [Output]
fullRun p inputs = run RunningProgram { position = 0, program = p, inputs = inputs, relativeBase = 0 }

run :: RunningProgram -> [Output]
run running
  | programHasEnded running = []
  | otherwise = stepOutputs ++ (run newRunning)
  where (newRunning, stepOutputs) = step running

step :: RunningProgram -> (RunningProgram, [Output])
step (running@RunningProgram { position = pos, program = program, inputs = inputs, relativeBase = relativeBase })
  | programHasEnded running = (running, [])
  | otherwise = (RunningProgram { position = newPos, program = newProgram, inputs = newInputs, relativeBase = newRelativeBase }, outputs)
  where (instruction, parType) = parseOperation $ readPosition pos program
        (nrParameters, nrInputs, pipeInput, outputType, fun) = instruction
        parameterPositions = readParameterPositions parType (pos + 1) program relativeBase
        newInputs = if pipeInput then drop nrInputs inputs else inputs
        stepInputs = take nrInputs (if pipeInput then inputs else map (\p -> readPosition p program) parameterPositions)
        stepOutputs = fun stepInputs
        newProgram = if outputType == OutputInProgram then changePosition (head stepOutputs) (last parameterPositions) program else program
        outputs = if outputType == OutputInPipe then stepOutputs else []
        newPos = if ((outputType == JumpPosition) && (not $ null stepOutputs)) then (head stepOutputs) else (pos + nrParameters + 1)
        newRelativeBase = if outputType == ChangeRelativeBase then relativeBase + (head stepOutputs) else relativeBase

parseOperation :: Int -> (Instruction, [ParameterType])
parseOperation i = (instruction, (take nrParameters (parseParameterModes (div i 100))))
  where parseParameterModes 0 = repeat PtPosition
        parseParameterModes x = (ptFromInt (mod x 10)) : (parseParameterModes (div x 10))
        (nrParameters, nrInputs, pipeInput, _, _) = instruction
        instruction = instructions Map.! opcode
        opcode = (mod i 100)
        ptFromInt 0 = PtPosition
        ptFromInt 1 = PtImmediate
        ptFromInt 2 = PtRelative

readParameterPositions :: [ParameterType] -> Position -> Program -> Position -> [Position]
readParameterPositions [] _ _ _ = []
readParameterPositions (x : xs) pos prog relativeBase = (read1 x) : (readParameterPositions xs (pos + 1) prog relativeBase)
  where
    par = readPosition pos prog
    read1 PtImmediate = pos
    read1 PtPosition = par
    read1 PtRelative = relativeBase + par

changePosition :: Value -> Position -> Program -> Program
changePosition v 0 (_ : tail) = v : tail
changePosition v p (h : tail) = h : (changePosition v (p - 1) tail)
changePosition v p [] = changePosition v p [0] 

readPosition :: Position -> Program -> Value
readPosition 0 (x : _) = x
readPosition p (h : t) = readPosition (p - 1) t
readPosition _ [] = 0

programHasEnded :: RunningProgram -> Bool
programHasEnded r
  | readPosition (position r) (program r) == 99 = True
  | otherwise = False

feedInput :: RunningProgram -> Input -> RunningProgram
feedInput (RunningProgram { position = pos, program = prog, inputs = inputs, relativeBase = base }) i = 
  RunningProgram { position = pos, program = prog, inputs = inputs ++ [i], relativeBase = base }

stepToOutput :: RunningProgram -> (RunningProgram, [Output])
stepToOutput running
  | programHasEnded running = (nrunning, [])
  | null out = stepToOutput nrunning
  | otherwise = (nrunning, out)
  where (nrunning, out) = step running

debug :: RunningProgram -> [(RunningProgram, [Output])]
debug r
  | programHasEnded r = []
  | otherwise = x : debug p
  where x@(p, o) = step r

programDifferences :: Program -> Program -> Position -> String
programDifferences [] [] _ = []
programDifferences p1 [] _ = "p1 longer " ++ show p1
programDifferences [] p2 _ = "p2 longer " ++ show p2
programDifferences (x1 : t1) (x2 : t2) pos
  | x1 == x2 = cont
  | x1 /= x2 = (show pos) ++ ": " ++ (show x1) ++ " > " ++ (show x2) ++ "\n" ++ cont
  where cont = programDifferences t1 t2 (pos + 1)

showDebug :: RunningProgram -> Int -> IO()
showDebug r nr = print $ take nr $ debug r
  where
    print [x] = putStrLn "end"
    print [] = putStrLn "end"
    print ((h1, o1) : (h2, o2) : t) = putStrLn (show h2) >> putStrLn (show o2) >> putStrLn (programDifferences (program h1) (program h2) 0) >> print ((h2, o2) : t)

createRP p i = RunningProgram { position = 0, program = p, inputs = i, relativeBase = 0 }
--day5Program = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,67,92,225,1101,14,84,225,1002,217,69,224,101,-5175,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1,214,95,224,101,-127,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,8,41,225,2,17,91,224,1001,224,-518,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,37,27,225,1101,61,11,225,101,44,66,224,101,-85,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,7,32,224,101,-224,224,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1001,14,82,224,101,-174,224,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,102,65,210,224,101,-5525,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,81,9,224,101,-90,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,71,85,225,1102,61,66,225,1102,75,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,226,224,102,2,223,223,1005,224,329,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,359,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,434,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,677,224,102,2,223,223,1006,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,599,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,644,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]
--day9Program = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,24,1017,1101,0,36,1006,1101,0,30,1011,1101,26,0,1018,1101,32,0,1015,1101,34,0,1004,1101,0,37,1002,1101,25,0,1012,1102,38,1,1010,1101,29,0,1019,1101,308,0,1029,1102,1,696,1027,1102,1,429,1022,1102,1,21,1005,1102,1,33,1013,1101,39,0,1008,1102,20,1,1009,1101,0,652,1025,1102,313,1,1028,1101,0,31,1003,1102,661,1,1024,1101,35,0,1016,1101,0,23,1000,1102,28,1,1014,1102,0,1,1020,1102,27,1,1007,1101,0,1,1021,1102,22,1,1001,1101,703,0,1026,1101,0,422,1023,109,-5,2101,0,9,63,1008,63,31,63,1005,63,205,1001,64,1,64,1105,1,207,4,187,1002,64,2,64,109,6,2102,1,3,63,1008,63,37,63,1005,63,227,1105,1,233,4,213,1001,64,1,64,1002,64,2,64,109,11,21108,40,40,3,1005,1015,255,4,239,1001,64,1,64,1106,0,255,1002,64,2,64,109,-3,21107,41,40,2,1005,1011,275,1001,64,1,64,1105,1,277,4,261,1002,64,2,64,109,4,2107,28,-6,63,1005,63,297,1001,64,1,64,1106,0,299,4,283,1002,64,2,64,109,15,2106,0,0,4,305,1106,0,317,1001,64,1,64,1002,64,2,64,109,-23,2108,22,4,63,1005,63,337,1001,64,1,64,1105,1,339,4,323,1002,64,2,64,109,6,21101,42,0,0,1008,1011,40,63,1005,63,363,1001,64,1,64,1105,1,365,4,345,1002,64,2,64,109,-17,1207,7,21,63,1005,63,381,1105,1,387,4,371,1001,64,1,64,1002,64,2,64,109,14,1201,-1,0,63,1008,63,25,63,1005,63,407,1105,1,413,4,393,1001,64,1,64,1002,64,2,64,109,15,2105,1,0,1001,64,1,64,1105,1,431,4,419,1002,64,2,64,109,-23,2101,0,6,63,1008,63,36,63,1005,63,453,4,437,1106,0,457,1001,64,1,64,1002,64,2,64,109,10,2108,21,-5,63,1005,63,475,4,463,1106,0,479,1001,64,1,64,1002,64,2,64,109,-3,1201,2,0,63,1008,63,20,63,1005,63,505,4,485,1001,64,1,64,1105,1,505,1002,64,2,64,109,4,2107,35,-5,63,1005,63,527,4,511,1001,64,1,64,1105,1,527,1002,64,2,64,109,15,1206,-5,543,1001,64,1,64,1105,1,545,4,533,1002,64,2,64,109,-8,1205,3,563,4,551,1001,64,1,64,1106,0,563,1002,64,2,64,109,-5,1206,7,581,4,569,1001,64,1,64,1105,1,581,1002,64,2,64,109,-8,1207,-3,38,63,1005,63,599,4,587,1105,1,603,1001,64,1,64,1002,64,2,64,109,19,1205,-4,619,1001,64,1,64,1105,1,621,4,609,1002,64,2,64,109,-13,1208,-4,27,63,1005,63,639,4,627,1105,1,643,1001,64,1,64,1002,64,2,64,109,5,2105,1,8,4,649,1001,64,1,64,1106,0,661,1002,64,2,64,109,-16,1202,4,1,63,1008,63,34,63,1005,63,683,4,667,1106,0,687,1001,64,1,64,1002,64,2,64,109,26,2106,0,1,1001,64,1,64,1105,1,705,4,693,1002,64,2,64,109,-9,21102,43,1,-7,1008,1010,46,63,1005,63,725,1105,1,731,4,711,1001,64,1,64,1002,64,2,64,109,-26,1202,9,1,63,1008,63,26,63,1005,63,755,1001,64,1,64,1105,1,757,4,737,1002,64,2,64,109,34,21108,44,43,-8,1005,1017,773,1106,0,779,4,763,1001,64,1,64,1002,64,2,64,109,-15,21102,45,1,1,1008,1011,45,63,1005,63,801,4,785,1106,0,805,1001,64,1,64,1002,64,2,64,109,-14,1208,10,35,63,1005,63,821,1106,0,827,4,811,1001,64,1,64,1002,64,2,64,109,17,2102,1,-4,63,1008,63,20,63,1005,63,853,4,833,1001,64,1,64,1106,0,853,1002,64,2,64,109,6,21107,46,47,-4,1005,1015,871,4,859,1105,1,875,1001,64,1,64,1002,64,2,64,109,-10,21101,47,0,4,1008,1013,47,63,1005,63,901,4,881,1001,64,1,64,1105,1,901,4,64,99,21102,27,1,1,21102,1,915,0,1106,0,922,21201,1,37790,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1106,0,922,22102,1,1,-1,21201,-2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0]
--day11ProgramC = [3, 8, 1005, 8, 301, 1106, 0, 11, 0, 0, 0, 104, 1, 104, 0, 3, 8, 102, -1, 8, 10, 1001, 10, 1, 10, 4, 10, 1008, 8, 0, 10, 4, 10, 1002, 8, 1, 29, 1, 1103, 7, 10, 3, 8, 102, -1, 8, 10, 101, 1, 10, 10, 4, 10, 108, 1, 8, 10, 4, 10, 1002, 8, 1, 54, 2, 103, 3, 10, 2, 1008, 6, 10, 1006, 0, 38, 2, 1108, 7, 10, 3, 8, 102, -1, 8, 10, 1001, 10, 1, 10, 4, 10, 108, 1, 8, 10, 4, 10, 1001, 8, 0, 91, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 1008, 8, 0, 10, 4, 10, 101, 0, 8, 114, 3, 8, 1002, 8, -1, 10, 101, 1, 10, 10, 4, 10, 1008, 8, 1, 10, 4, 10, 1001, 8, 0, 136, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 1008, 8, 1, 10, 4, 10, 1002, 8, 1, 158, 1, 1009, 0, 10, 2, 1002, 18, 10, 3, 8, 102, -1, 8, 10, 101, 1, 10, 10, 4, 10, 108, 0, 8, 10, 4, 10, 1002, 8, 1, 187, 2, 1108, 6, 10, 3, 8, 1002, 8, -1, 10, 1001, 10, 1, 10, 4, 10, 108, 0, 8, 10, 4, 10, 1002, 8, 1, 213, 3, 8, 1002, 8, -1, 10, 101, 1, 10, 10, 4, 10, 1008, 8, 1, 10, 4, 10, 1001, 8, 0, 236, 1, 104, 10, 10, 1, 1002, 20, 10, 2, 1008, 9, 10, 3, 8, 102, -1, 8, 10, 101, 1, 10, 10, 4, 10, 108, 0, 8, 10, 4, 10, 101, 0, 8, 269, 1, 102, 15, 10, 1006, 0, 55, 2, 1107, 15, 10, 101, 1, 9, 9, 1007, 9, 979, 10, 1005, 10, 15, 99, 109, 623, 104, 0, 104, 1, 21102, 1, 932700598932, 1, 21102, 318, 1, 0, 1105, 1, 422, 21102, 1, 937150489384, 1, 21102, 329, 1, 0, 1105, 1, 422, 3, 10, 104, 0, 104, 1, 3, 10, 104, 0, 104, 0, 3, 10, 104, 0, 104, 1, 3, 10, 104, 0, 104, 1, 3, 10, 104, 0, 104, 0, 3, 10, 104, 0, 104, 1, 21101, 46325083227, 0, 1, 21102, 376, 1, 0, 1106, 0, 422, 21102, 3263269927, 1, 1, 21101, 387, 0, 0, 1105, 1, 422, 3, 10, 104, 0, 104, 0, 3, 10, 104, 0, 104, 0, 21102, 988225102184, 1, 1, 21101, 410, 0, 0, 1105, 1, 422, 21101, 868410356500, 0, 1, 21102, 1, 421, 0, 1106, 0, 422, 99, 109, 2, 21202, -1, 1, 1, 21102, 1, 40, 2, 21102, 1, 453, 3, 21102, 1, 443, 0, 1105, 1, 486, 109, -2, 2106, 0, 0, 0, 1, 0, 0, 1, 109, 2, 3, 10, 204, -1, 1001, 448, 449, 464, 4, 0, 1001, 448, 1, 448, 108, 4, 448, 10, 1006, 10, 480, 1102, 1, 0, 448, 109, -2, 2106, 0, 0, 0, 109, 4, 1201, -1, 0, 485, 1207, -3, 0, 10, 1006, 10, 503, 21101, 0, 0, -3, 22101, 0, -3, 1, 21201, -2, 0, 2, 21102, 1, 1, 3, 21101, 0, 522, 0, 1105, 1, 527, 109, -4, 2106, 0, 0, 109, 5, 1207, -3, 1, 10, 1006, 10, 550, 2207, -4, -2, 10, 1006, 10, 550, 22102, 1, -4, -4, 1105, 1, 618, 21201, -4, 0, 1, 21201, -3, -1, 2, 21202, -2, 2, 3, 21102, 569, 1, 0, 1106, 0, 527, 22101, 0, 1, -4, 21101, 0, 1, -1, 2207, -4, -2, 10, 1006, 10, 588, 21102, 1, 0, -1, 22202, -2, -1, -2, 2107, 0, -3, 10, 1006, 10, 610, 21201, -1, 0, 1, 21101, 610, 0, 0, 105, 1, 485, 21202, -2, -1, -2, 22201, -4, -2, -4, 109, -5, 2105, 1, 0]
--r11 = createRP day11ProgramC [0,0]




