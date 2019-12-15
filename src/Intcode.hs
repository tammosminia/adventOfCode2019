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
instructions = Map.fromList [(1, plus), (2, product), (3, pipeIn), (4, pipeOut), (5, ifTrue), (6, ifFalse), (7, lessThan), (8, equals), (9, changeRelativeBase), (99, end)]
  where
    plus = (3, 2, False, OutputInProgram, (\[a,b] -> [a + b]))
    product = (3, 2, False, OutputInProgram, (\[a,b] -> [a * b]))
    pipeIn = (1, 1, True, OutputInProgram, id)
    pipeOut = (1, 1, False, OutputInPipe, id)
    ifTrue = (2, 2, False, JumpPosition, (\[x, p] -> if (x /= 0) then [p] else []))
    ifFalse = (2, 2, False, JumpPosition, (\[x, p] -> if (x == 0) then [p] else []))
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
        newPos = if (outputType == JumpPosition) && (not $ null stepOutputs) then head stepOutputs else pos + nrParameters + 1
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
