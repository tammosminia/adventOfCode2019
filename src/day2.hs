input :: Program
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,10,23,27,2,27,13,31,1,31,6,35,2,6,35,39,1,39,5,43,1,6,43,47,2,6,47,51,1,51,5,55,2,55,9,59,1,6,59,63,1,9,63,67,1,67,10,71,2,9,71,75,1,6,75,79,1,5,79,83,2,83,10,87,1,87,5,91,1,91,9,95,1,6,95,99,2,99,10,103,1,103,5,107,2,107,6,111,1,111,5,115,1,9,115,119,2,119,10,123,1,6,123,127,2,13,127,131,1,131,6,135,1,135,10,139,1,13,139,143,1,143,13,147,1,5,147,151,1,151,2,155,1,155,5,0,99,2,0,14,0]

type Program = [Int]
type Position = Int
type Value = Int

fullRun :: Program -> Program
fullRun p = run 0 p

run :: Position -> Program -> Program
run pos program
  | opcode == 99 = program
  | opcode == 1 = cont plus
  | opcode == 2 = cont times
  where opcode = program !! pos
        cont f = run (pos + 4) (calculate pos program f)

plus :: Value -> Value -> Value
plus x1 x2 = x1 + x2

times :: Value -> Value -> Value
times x1 x2 = x1 * x2

parameters :: Position -> Program -> (Value, Value, Position)
parameters position program =
  let readPosition x = program !! (position + x)
      readValue x = program !! (readPosition x)
  in (readValue 1, readValue 2, readPosition 3)

writeResult :: Value -> Position -> Program -> Program
writeResult v 0 (_ : tail) = v : tail
writeResult v p (h : tail) = h : (writeResult v (p - 1) tail)

calculate :: Position -> Program -> (Value -> Value -> Value) -> Program
calculate pos prog fun = let (x1, x2, outPos) = (parameters pos prog)
                             result = fun x1 x2
                         in writeResult result outPos prog

answer1 = fullRun input

-- part 2
wanted :: Value
wanted = 19690720

staticInput = tail (tail (tail input))

runFor :: Value -> Value -> Value
runFor noun verb = let prog = 1 : noun : verb : staticInput
                   in head (fullRun prog)

run2 :: [Value]
run2 = [noun * 100 + verb | noun <- [0..99], verb <- [0..99], (runFor noun verb) == wanted]

answer2 = head run2