import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day7
import Intcode

--testProg :: Program -> [Input] -> [Output] ->
testProg prog input output = fullRun prog input `shouldBe` output

createRP p i = RunningProgram { position = 0, program = p, inputs = i, relativeBase = 0 }

day5Program = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,67,92,225,1101,14,84,225,1002,217,69,224,101,-5175,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1,214,95,224,101,-127,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,8,41,225,2,17,91,224,1001,224,-518,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,37,27,225,1101,61,11,225,101,44,66,224,101,-85,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,7,32,224,101,-224,224,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1001,14,82,224,101,-174,224,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,102,65,210,224,101,-5525,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,81,9,224,101,-90,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,71,85,225,1102,61,66,225,1102,75,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,226,224,102,2,223,223,1005,224,329,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,359,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,434,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,677,224,102,2,223,223,1006,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,599,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,644,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]
day9Program = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,24,1017,1101,0,36,1006,1101,0,30,1011,1101,26,0,1018,1101,32,0,1015,1101,34,0,1004,1101,0,37,1002,1101,25,0,1012,1102,38,1,1010,1101,29,0,1019,1101,308,0,1029,1102,1,696,1027,1102,1,429,1022,1102,1,21,1005,1102,1,33,1013,1101,39,0,1008,1102,20,1,1009,1101,0,652,1025,1102,313,1,1028,1101,0,31,1003,1102,661,1,1024,1101,35,0,1016,1101,0,23,1000,1102,28,1,1014,1102,0,1,1020,1102,27,1,1007,1101,0,1,1021,1102,22,1,1001,1101,703,0,1026,1101,0,422,1023,109,-5,2101,0,9,63,1008,63,31,63,1005,63,205,1001,64,1,64,1105,1,207,4,187,1002,64,2,64,109,6,2102,1,3,63,1008,63,37,63,1005,63,227,1105,1,233,4,213,1001,64,1,64,1002,64,2,64,109,11,21108,40,40,3,1005,1015,255,4,239,1001,64,1,64,1106,0,255,1002,64,2,64,109,-3,21107,41,40,2,1005,1011,275,1001,64,1,64,1105,1,277,4,261,1002,64,2,64,109,4,2107,28,-6,63,1005,63,297,1001,64,1,64,1106,0,299,4,283,1002,64,2,64,109,15,2106,0,0,4,305,1106,0,317,1001,64,1,64,1002,64,2,64,109,-23,2108,22,4,63,1005,63,337,1001,64,1,64,1105,1,339,4,323,1002,64,2,64,109,6,21101,42,0,0,1008,1011,40,63,1005,63,363,1001,64,1,64,1105,1,365,4,345,1002,64,2,64,109,-17,1207,7,21,63,1005,63,381,1105,1,387,4,371,1001,64,1,64,1002,64,2,64,109,14,1201,-1,0,63,1008,63,25,63,1005,63,407,1105,1,413,4,393,1001,64,1,64,1002,64,2,64,109,15,2105,1,0,1001,64,1,64,1105,1,431,4,419,1002,64,2,64,109,-23,2101,0,6,63,1008,63,36,63,1005,63,453,4,437,1106,0,457,1001,64,1,64,1002,64,2,64,109,10,2108,21,-5,63,1005,63,475,4,463,1106,0,479,1001,64,1,64,1002,64,2,64,109,-3,1201,2,0,63,1008,63,20,63,1005,63,505,4,485,1001,64,1,64,1105,1,505,1002,64,2,64,109,4,2107,35,-5,63,1005,63,527,4,511,1001,64,1,64,1105,1,527,1002,64,2,64,109,15,1206,-5,543,1001,64,1,64,1105,1,545,4,533,1002,64,2,64,109,-8,1205,3,563,4,551,1001,64,1,64,1106,0,563,1002,64,2,64,109,-5,1206,7,581,4,569,1001,64,1,64,1105,1,581,1002,64,2,64,109,-8,1207,-3,38,63,1005,63,599,4,587,1105,1,603,1001,64,1,64,1002,64,2,64,109,19,1205,-4,619,1001,64,1,64,1105,1,621,4,609,1002,64,2,64,109,-13,1208,-4,27,63,1005,63,639,4,627,1105,1,643,1001,64,1,64,1002,64,2,64,109,5,2105,1,8,4,649,1001,64,1,64,1106,0,661,1002,64,2,64,109,-16,1202,4,1,63,1008,63,34,63,1005,63,683,4,667,1106,0,687,1001,64,1,64,1002,64,2,64,109,26,2106,0,1,1001,64,1,64,1105,1,705,4,693,1002,64,2,64,109,-9,21102,43,1,-7,1008,1010,46,63,1005,63,725,1105,1,731,4,711,1001,64,1,64,1002,64,2,64,109,-26,1202,9,1,63,1008,63,26,63,1005,63,755,1001,64,1,64,1105,1,757,4,737,1002,64,2,64,109,34,21108,44,43,-8,1005,1017,773,1106,0,779,4,763,1001,64,1,64,1002,64,2,64,109,-15,21102,45,1,1,1008,1011,45,63,1005,63,801,4,785,1106,0,805,1001,64,1,64,1002,64,2,64,109,-14,1208,10,35,63,1005,63,821,1106,0,827,4,811,1001,64,1,64,1002,64,2,64,109,17,2102,1,-4,63,1008,63,20,63,1005,63,853,4,833,1001,64,1,64,1106,0,853,1002,64,2,64,109,6,21107,46,47,-4,1005,1015,871,4,859,1105,1,875,1001,64,1,64,1002,64,2,64,109,-10,21101,47,0,4,1008,1013,47,63,1005,63,901,4,881,1001,64,1,64,1105,1,901,4,64,99,21102,27,1,1,21102,1,915,0,1106,0,922,21201,1,37790,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1106,0,922,22102,1,1,-1,21201,-2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0]

main :: IO ()
main = hspec $ do
  describe "testInstructions" $ do
    it "plus positional" $ do
      step (createRP [1,2,4,5,99] []) `shouldBe` (RunningProgram { position = 4, program = [1,2,4,5,99,103], inputs = [], relativeBase = 0 }, [])
    it "plus immediate inputs" $ do
      step (createRP [1101,2,4,5,99] []) `shouldBe` (RunningProgram { position = 4, program = [1101,2,4,5,99,6], inputs = [], relativeBase = 0 }, [])
    it "plus immediate inputs and outputs" $ do
      step (createRP [11101,2,4,5,99] []) `shouldBe` (RunningProgram { position = 4, program = [11101,2,4,6,99], inputs = [], relativeBase = 0 }, [])
    it "plus relative inputs" $ do
      step RunningProgram { position = 0, program = [2201,1,2,5,99,11,22], inputs = [], relativeBase = 4 } `shouldBe` (RunningProgram { position = 4, program = [2201,1,2,5,99,33,22], inputs = [], relativeBase = 4 }, [])
    it "plus relative inputs and outputs" $ do
      step RunningProgram { position = 0, program = [22201,1,2,5,99,11,22], inputs = [], relativeBase = 4 } `shouldBe` (RunningProgram { position = 4, program = [22201,1,2,5,99,11,22,0,0,33], inputs = [], relativeBase = 4 }, [])

    it "pipeIn positional" $ do
      step (createRP [3,2] [9]) `shouldBe` (RunningProgram { position = 2, program = [3,2,9], inputs = [], relativeBase = 0 }, [])
    it "pipeIn immediate" $ do
      step (createRP [103,2] [9]) `shouldBe` (RunningProgram { position = 2, program = [103,9], inputs = [], relativeBase = 0 }, [])
    it "pipeIn relative" $ do
      step RunningProgram { position = 0, program = [203,2], inputs = [9], relativeBase = 1 } `shouldBe` (RunningProgram { position = 2, program = [203,2,0,9], inputs = [], relativeBase = 1 }, [])

    it "pipeOut positional" $ do
      step (createRP [4,2,99] []) `shouldBe` (RunningProgram { position = 2, program = [4,2,99], inputs = [], relativeBase = 0 }, [99])
    it "pipeOut positional" $ do
      step (createRP [4,0,99] []) `shouldBe` (RunningProgram { position = 2, program = [4,0,99], inputs = [], relativeBase = 0 }, [4])
    it "pipeOut immediate" $ do
      step (createRP [104,2,99] []) `shouldBe` (RunningProgram { position = 2, program = [104,2,99], inputs = [], relativeBase = 0 }, [2])
    it "pipeOut relative" $ do
      step RunningProgram { position = 0, program = [204,2,99,7], inputs = [], relativeBase = 1 } `shouldBe` (RunningProgram { position = 2, program = [204,2,99,7], inputs = [], relativeBase = 1 }, [7])

    it "jumpTrue positional" $ do
      step (createRP [5,1,3,99] []) `shouldBe` (RunningProgram { position = 99, program = [5,1,3,99], inputs = [], relativeBase = 0 }, [])
      step (createRP [5,1,0,99] []) `shouldBe` (RunningProgram { position = 5, program = [5,1,0,99], inputs = [], relativeBase = 0 }, [])
      step (createRP [5,2,0,99] []) `shouldBe` (RunningProgram { position = 3, program = [5,2,0,99], inputs = [], relativeBase = 0 }, [])
    it "jumpTrue immediate" $ do
      step (createRP [1105,1,5,99] []) `shouldBe` (RunningProgram { position = 5, program = [1105,1,5,99], inputs = [], relativeBase = 0 }, [])
      step (createRP [1105,0,0,99] []) `shouldBe` (RunningProgram { position = 3, program = [1105,0,0,99], inputs = [], relativeBase = 0 }, [])
    it "jumpTrue relative" $ do
      step RunningProgram { position = 0, program = [2205,1,2,99,1,9], inputs = [], relativeBase = 3 } `shouldBe` (RunningProgram { position = 9, program = [2205,1,2,99,1,9], inputs = [], relativeBase = 3 }, [])
      step RunningProgram { position = 0, program = [2205,1,2,99,0,9], inputs = [], relativeBase = 3 } `shouldBe` (RunningProgram { position = 3, program = [2205,1,2,99,0,9], inputs = [], relativeBase = 3 }, [])

    it "lessThan positional" $ do
      step (createRP [7,1,0,0,99] []) `shouldBe` (RunningProgram { position = 4, program = [1,1,0,0,99], inputs = [], relativeBase = 0 }, [])
      step (createRP [7,1,1,0,99] []) `shouldBe` (RunningProgram { position = 4, program = [0,1,1,0,99], inputs = [], relativeBase = 0 }, [])
    it "lessThan immediate" $ do
      step (createRP [11107,2,3,5,99] []) `shouldBe` (RunningProgram { position = 4, program = [11107,2,3,1,99], inputs = [], relativeBase = 0 }, [])
      step (createRP [11107,3,3,5,99] []) `shouldBe` (RunningProgram { position = 4, program = [11107,3,3,0,99], inputs = [], relativeBase = 0 }, [])
    it "lessThan relative" $ do
      step RunningProgram { position = 0, program = [22207,1,2,3,99,6,7,5], inputs = [], relativeBase = 4 } `shouldBe` (RunningProgram { position = 4, program = [22207,1,2,3,99,6,7,1], inputs = [], relativeBase = 4 }, [])
      step RunningProgram { position = 0, program = [22207,1,2,3,99,6,6,5], inputs = [], relativeBase = 4 } `shouldBe` (RunningProgram { position = 4, program = [22207,1,2,3,99,6,6,0], inputs = [], relativeBase = 4 }, [])

  describe "day5" $ do
    it "fullRun" $ do
      testProg [1002,4,3,4,33] [] []
      testProg [3,0,4,0,99] [13] [13]

    it "compareTo8" $ do
      testProg [3,9,8,9,10,9,4,9,99,-1,8] [8] [1]
      testProg [3,9,8,9,10,9,4,9,99,-1,8] [7] [0]
      testProg [3,3,1108,-1,8,3,4,3,99] [8] [1]
      testProg [3,3,1108,-1,8,3,4,3,99] [7] [0]

    it "lessThan8" $ do
      testProg [3,9,7,9,10,9,4,9,99,-1,8] [8] [0]
      testProg [3,9,7,9,10,9,4,9,99,-1,8] [7] [1]
      testProg [3,3,1107,-1,8,3,4,3,99] [8] [0]
      testProg [3,3,1107,-1,8,3,4,3,99] [7] [1]

    it "isNonZero1" $ do
      testProg [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0] [0]
    it "isNonZero2" $ do
      testProg [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [7] [1]
    it "isNonZero3" $ do
      testProg [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0] [0]
    it "isNonZero4" $ do
      testProg [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [7] [1]

    it "compare8" $ do
      let p = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
      testProg p [7] [999]
      testProg p [8] [1000]
      testProg p [9] [1001]

    it "part1" $ do
      testProg day5Program [1] [0,0,0,0,0,0,0,0,0,5074395]
    it "part2" $ do
      testProg day5Program [5] [8346937]

  describe "day7" $ do
    it "amplify" $ do
      amplify [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0] `shouldBe` 43210
      amplify [3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4] `shouldBe` 54321
      amplify [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2] `shouldBe` 65210

    it "amplify2" $ do
      amplify2max [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5] `shouldBe` 139629729
      amplify2max [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                   -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                   53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9,7,8,5,6] `shouldBe` 18216


  describe "day9" $ do
    it "outputprog" $ do
      let p = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
      testProg p [] p

    it "largeNumber" $ do
      head (fullRun [1102,34915192,34915192,7,4,7,99,0] []) `shouldSatisfy` (>10^15)
      testProg [104,1125899906842624,99] [] [1125899906842624]

    it "part1" $ do
      testProg day9Program [1] [2671328082]
