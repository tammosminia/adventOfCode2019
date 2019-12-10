module Day5 where

program1 :: Program
program1 = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,67,92,225,1101,14,84,225,1002,217,69,224,101,-5175,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1,214,95,224,101,-127,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,8,41,225,2,17,91,224,1001,224,-518,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,37,27,225,1101,61,11,225,101,44,66,224,101,-85,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,7,32,224,101,-224,224,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1001,14,82,224,101,-174,224,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,102,65,210,224,101,-5525,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,81,9,224,101,-90,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,71,85,225,1102,61,66,225,1102,75,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,226,224,102,2,223,223,1005,224,329,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,359,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,434,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,677,224,102,2,223,223,1006,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,599,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,644,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]

type Program = [Int]
type Position = Int
type Value = Int
type Output = (Program, [Value])
type Opcode = Int
type ParametersImmediate = [Bool]
type Operation = (Opcode, ParametersImmediate, Int) -- how many output parameters

fullRun :: Program -> [Value] -> Output
fullRun p inputs = run 0 p inputs

run :: Position -> Program -> [Value] -> Output
run pos program inputs
  | opcode == 99 = (program, [])
  | opcode == 1 = cont (writeOutput (sum inputParamaters)) inputs
  | opcode == 2 = cont (writeOutput (product inputParamaters)) inputs
  | opcode == 3 = cont (writeOutput (head inputs)) (tail inputs)
  | opcode == 4 = let (p, o) = (cont program inputs)
                  in (p, (head inputParamaters) : o)
  | opcode == 5 = if ((head inputParamaters) /= 0) then (run (inputParamaters !! 1) program inputs) else cont program inputs
  | opcode == 6 = if ((head inputParamaters) == 0) then (run (inputParamaters !! 1) program inputs) else cont program inputs
  | opcode == 7 = let isLess = if (inputParamaters !! 0) < (inputParamaters !! 1) then 1 else 0
                  in cont (writeOutput isLess) inputs
  | opcode == 8 = let isEqual = if (inputParamaters !! 0) == (inputParamaters !! 1) then 1 else 0
                  in cont (writeOutput isEqual) inputs
  where (opcode, inputPI, outputP) = parseOperation (program !! pos)
        outputPos = program !! (pos + (length inputPI) + 1)
        writeOutput x = changeProgram x outputPos program
        totalNrParams = (length inputPI) + outputP
        inputParamaters = readInputParameters inputPI (pos + 1) program
        cont prog is = run (pos + totalNrParams + 1) prog is

parseOperation :: Int -> Operation
parseOperation i = (opcode, (take amountOfInputParameters (parseParameterModes (div i 100))), amountOfOutputParameters)
  where parseParameterModes 0 = repeat False
        parseParameterModes x =  ((mod x 10) == 1) : (parseParameterModes (div x 10))
        amountOfInputParameters
          | opcode == 1 = 2
          | opcode == 2 = 2
          | opcode == 3 = 0
          | opcode == 4 = 1
          | opcode == 5 = 2
          | opcode == 6 = 2
          | opcode == 7 = 2
          | opcode == 8 = 2
          | opcode == 99 = 0
        amountOfOutputParameters
          | opcode == 1 = 1
          | opcode == 2 = 1
          | opcode == 3 = 1
          | opcode == 4 = 0
          | opcode == 5 = 0
          | opcode == 6 = 0
          | opcode == 7 = 1
          | opcode == 8 = 1
          | opcode == 99 = 0
        opcode = (mod i 100)

readInputParameters :: ParametersImmediate -> Position -> Program -> [Value]
readInputParameters [] _ _ = []
readInputParameters (True : xs) pos prog = (prog !! pos) : (readInputParameters xs (pos + 1) prog)
readInputParameters (False : xs) pos prog = (prog !! (prog !! pos)) : (readInputParameters xs (pos + 1) prog)

changeProgram :: Value -> Position -> Program -> Program
changeProgram v 0 (_ : tail) = v : tail
changeProgram v p (h : tail) = h : (changeProgram v (p - 1) tail)

runProgram1 = fullRun program1 [1]
answer1 = last (snd runProgram1)

runProgram2 = fullRun program1 [5]
answer2 = last (snd runProgram2)

