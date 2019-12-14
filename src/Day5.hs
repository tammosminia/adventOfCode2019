module Day5 where

import qualified Data.Map as Map

program1 :: Program
program1 = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,67,92,225,1101,14,84,225,1002,217,69,224,101,-5175,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1,214,95,224,101,-127,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,8,41,225,2,17,91,224,1001,224,-518,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,37,27,225,1101,61,11,225,101,44,66,224,101,-85,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,7,32,224,101,-224,224,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1001,14,82,224,101,-174,224,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,102,65,210,224,101,-5525,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,81,9,224,101,-90,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,71,85,225,1102,61,66,225,1102,75,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,226,224,102,2,223,223,1005,224,329,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,359,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,434,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,677,224,102,2,223,223,1006,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,599,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,644,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]

type Program = [Int]
type Position = Int
type Value = Int
type Opcode = Int
type NrInputs = Int
type PipeInput = Bool
type OutputType = Int -- 0 = Nothing(nr=0), 1 = write in program(nr=1), 2 = output in pipe(nr=1), 3 = jump position (nr=0/1)
type JumpOutput = Bool
type Input = Value
type Output = Value
type InputIsImmediate = Bool
type Instruction = (NrInputs, PipeInput, OutputType, [Input] -> [Output])


instructions :: Map.Map Opcode Instruction
instructions = Map.fromList [(1, plus), (2, product), (3, pipeIn), (4, pipeOut), (5, ifTrue), (6, ifFalse), (7, lessThan), (8, equals), (99, end)]
  where
    plus = (2, False, 1, (\[a,b] -> [a + b]))
    product = (2, False, 1, (\[a,b] -> [a * b]))
    pipeIn = (1, True, 1, (\x -> x))
    pipeOut = (1, False, 2, (\x -> x))
    ifTrue = (2, False, 3, (\[x, p] -> if (x /= 0) then [p] else []))
    ifFalse = (2, False, 3, (\[x, p] -> if (x == 0) then [p] else []))
    lessThan = (2, False, 1, (\[a, b] -> if (a < b) then [1] else [0]))
    equals = (2, False, 1, (\[a, b] -> if (a == b) then [1] else [0]))
    end = (0, False, 0, (\x -> x))

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
        nrOutputParameters = if outputType == 1 then 1 else 0
        newInputs = if pipeInput then drop nrInputs inputs else inputs
        stepInputs = if pipeInput then take nrInputs inputs else readInputParameters inputsImmediate (pos + 1) program
        stepOutputs = fun stepInputs
        newProgram = if outputType == 1 then changeProgram (head stepOutputs) outputPos program else program
        outputPos = program !! (pos + nrInputParameters + 1)
        outputs = if outputType == 2 then stepOutputs else []
        newPos = if (outputType == 3) && (not $ null stepOutputs) then head stepOutputs else pos + nrInputParameters + nrOutputParameters + 1

parseOperation :: Int -> (Instruction, [InputIsImmediate])
parseOperation i = (instruction, (take nrInputParameters (parseParameterModes (div i 100))))
  where parseParameterModes 0 = repeat False
        parseParameterModes x = ((mod x 10) == 1) : (parseParameterModes (div x 10))
        (nrInputs, pipeInput, _, _) = instruction
        nrInputParameters = if pipeInput then 0 else nrInputs
        instruction = instructions Map.! opcode
        opcode = (mod i 100)

readInputParameters :: [InputIsImmediate] -> Position -> Program -> [Value]
readInputParameters [] _ _ = []
readInputParameters (True : xs) pos prog = (prog !! pos) : (readInputParameters xs (pos + 1) prog)
readInputParameters (False : xs) pos prog = (prog !! (prog !! pos)) : (readInputParameters xs (pos + 1) prog)

changeProgram :: Value -> Position -> Program -> Program
changeProgram v 0 (_ : tail) = v : tail
changeProgram v p (h : tail) = h : (changeProgram v (p - 1) tail)

programHasEnded :: Position -> Program -> Bool
programHasEnded pos program
  | program !! pos == 99 = True
  | otherwise = False

runProgram1 = fullRun program1 [1]
answer1 = last (snd runProgram1)

runProgram2 = fullRun program1 [5]
answer2 = last (snd runProgram2)

