module Intcode where

import qualified Data.Map as Map

type Program = [Int]
type Position = Int
type Value = Int
type Opcode = Int
type NrInputs = Int
type PipeInput = Bool
data OutputType = OutputNothing | OutputInProgram | OutputInPipe | JumpPosition deriving (Show, Eq) -- Nothing(nr=0), write in program(nr=1), output in pipe(nr=1), jump position (nr=0/1)
type JumpOutput = Bool
type Input = Value
type Output = Value
data ParameterType = PtImmediate | PtPosition | PtRelative deriving (Show)
type Instruction = (NrInputs, PipeInput, OutputType, [Input] -> [Output])
data RunningProgram = RunningProgram { position :: Position, program :: Program, inputs :: [Input] } deriving (Show)


instructions :: Map.Map Opcode Instruction
instructions = Map.fromList [(1, plus), (2, product), (3, pipeIn), (4, pipeOut), (5, ifTrue), (6, ifFalse), (7, lessThan), (8, equals), (99, end)]
  where
    plus = (2, False, OutputInProgram, (\[a,b] -> [a + b]))
    product = (2, False, OutputInProgram, (\[a,b] -> [a * b]))
    pipeIn = (1, True, OutputInProgram, (\x -> x))
    pipeOut = (1, False, OutputInPipe, (\x -> x))
    ifTrue = (2, False, JumpPosition, (\[x, p] -> if (x /= 0) then [p] else []))
    ifFalse = (2, False, JumpPosition, (\[x, p] -> if (x == 0) then [p] else []))
    lessThan = (2, False, OutputInProgram, (\[a, b] -> if (a < b) then [1] else [0]))
    equals = (2, False, OutputInProgram, (\[a, b] -> if (a == b) then [1] else [0]))
    end = (0, False, OutputNothing, (\x -> x))

fullRun :: Program -> [Value] -> (Program, [Output])
fullRun p inputs = run 0 p inputs

run :: Position -> Program -> [Value] -> (Program, [Output])
run pos program inputs
  | programHasEnded pos program = (program, [])
  | otherwise = let (p, o) = run newPos newProg newInputs
                in (p, stepOutputs ++ o)
  where (newPos, newProg, newInputs, stepOutputs) = step pos program inputs

step :: Position -> Program -> [Input] -> (Position, Program, [Input], [Output])
step pos program inputs
  | programHasEnded pos program = (0, program, inputs, [])
  | otherwise = (newPos, newProgram, newInputs, outputs)
  where (instruction, inputsImmediate) = parseOperation $ program !! pos
        (nrInputs, pipeInput, outputType, fun) = instruction
        nrInputParameters = length inputsImmediate
        nrOutputParameters = if outputType == OutputInProgram then 1 else 0
        newInputs = if pipeInput then drop nrInputs inputs else inputs
        stepInputs = if pipeInput then take nrInputs inputs else readInputParameters inputsImmediate (pos + 1) program
        stepOutputs = fun stepInputs
        newProgram = if outputType == OutputInProgram then changeProgram (head stepOutputs) outputPos program else program
        outputPos = program !! (pos + nrInputParameters + 1)
        outputs = if outputType == OutputInPipe then stepOutputs else []
        newPos = if (outputType == JumpPosition) && (not $ null stepOutputs) then head stepOutputs else pos + nrInputParameters + nrOutputParameters + 1

parseOperation :: Int -> (Instruction, [ParameterType])
parseOperation i = (instruction, (take nrInputParameters (parseParameterModes (div i 100))))
  where parseParameterModes 0 = repeat PtPosition
        parseParameterModes x = (ptFromInt (mod x 10)) : (parseParameterModes (div x 10))
        (nrInputs, pipeInput, _, _) = instruction
        nrInputParameters = if pipeInput then 0 else nrInputs
        instruction = instructions Map.! opcode
        opcode = (mod i 100)
        ptFromInt 0 = PtPosition
        ptFromInt 1 = PtImmediate
        ptFromInt 2 = PtRelative

readInputParameters :: [ParameterType] -> Position -> Program -> [Value]
readInputParameters [] _ _ = []
readInputParameters (x : xs) pos prog = (read1 x) : (readInputParameters xs (pos + 1) prog)
  where
    read1 PtImmediate = (prog !! pos)
    read1 PtPosition = (prog !! (prog !! pos))

changeProgram :: Value -> Position -> Program -> Program
changeProgram v 0 (_ : tail) = v : tail
changeProgram v p (h : tail) = h : (changeProgram v (p - 1) tail)

programHasEnded :: Position -> Program -> Bool
programHasEnded pos program
  | program !! pos == 99 = True
  | otherwise = False

feedInput :: RunningProgram -> Input -> RunningProgram
feedInput (RunningProgram { position = pos, program = prog, inputs = inputs }) i = RunningProgram { position = pos, program = prog, inputs = inputs ++ [i] }

stepToOutput :: RunningProgram -> (RunningProgram, [Output])
stepToOutput (RunningProgram { position = pos, program = prog, inputs = inputs })
  | programHasEnded pos prog = (running, [])
  | null out = stepToOutput running
  | otherwise = (running, out)
  where (npos, nprog, nins, out) = step pos prog inputs
        running = RunningProgram { position = npos, program = nprog, inputs = nins }
