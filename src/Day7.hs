module Day7 where

import Day5
import Data.List

ampProgram1 = [3,8,1001,8,10,8,105,1,0,0,21,34,55,68,93,106,187,268,349,430,99999,3,9,102,5,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,102,2,9,9,101,2,9,9,102,2,9,9,4,9,99,3,9,101,2,9,9,102,4,9,9,4,9,99,3,9,101,4,9,9,102,3,9,9,1001,9,2,9,102,4,9,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,1002,9,5,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99]

amplify :: Program -> [Value] -> Value
amplify program phaseSettings = amp phaseSettings 0
  where
    amp [] v = v
    amp (a : as) v = amp as (runAmp a v)
    runAmp a v = head $ snd $ fullRun program [a, v] 
    
power1 = map (amplify ampProgram1) (permutations [0..4])
answer1 = maximum power1

type RunningProgram = (Position, Program, [Input])

amplify2 :: Program -> [Value] -> [Output]
amplify2 program phaseSettings = amp initAmps 0
  where
    initAmps :: [RunningProgram]
    initAmps = map (initAmplifier program) phaseSettings 
    amp :: [RunningProgram] -> Value -> [Output]
    amp amps v
      | null out = []
      | otherwise = outv : amp namps outv
      where (namps, out) = oneLoop amps v
            [outv] = out
    oneLoop amps v = foldl oneLoopF ([], [v]) amps
    oneLoopF (amps, []) amp = (amps, [])
    oneLoopF (amps, [v]) amp = (amps ++ [namp], out)
      where (namp, out) = stepToOutput $ feedInput amp v
    
amplify2max :: Program -> [Value] -> Output
amplify2max program phaseSettings = maximum $ amplify2 program phaseSettings

initAmplifier :: Program -> Value -> RunningProgram
initAmplifier p phaseSetting = (0, p, [phaseSetting])

feedInput :: RunningProgram -> Input -> RunningProgram
feedInput (pos, prog, inputs) i = (pos, prog, inputs ++ [i])

stepToOutput :: RunningProgram -> (RunningProgram, [Output])
stepToOutput (pos, prog, inputs)
  | programHasEnded pos prog = (running, [])
  | null out = stepToOutput running
  | otherwise = (running, out)
  where (npos, nprog, nins, out) = step pos prog inputs
        running = (npos, nprog, nins) 
  
power2 = map (amplify2max ampProgram1) (permutations [5..9])
answer2 = maximum power2
   