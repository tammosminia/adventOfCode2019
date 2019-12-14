import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day7
import Intcode

main :: IO ()
main = hspec $ do

  describe "day5" $ do
    it "fullRun" $ do
      snd (fullRun [1002,4,3,4,33] []) `shouldBe` []
      snd (fullRun [1103,0,4,0,99] [13]) `shouldBe` [13]

    it "compareTo8" $ do
      snd (fullRun [3,9,8,9,10,9,4,9,99,-1,8] [8]) `shouldBe` [1]
      snd (fullRun [3,9,8,9,10,9,4,9,99,-1,8] [7]) `shouldBe` [0]
      snd (fullRun [3,3,1108,-1,8,3,4,3,99] [8]) `shouldBe` [1]
      snd (fullRun [3,3,1108,-1,8,3,4,3,99] [7]) `shouldBe` [0]

    it "lessThan8" $ do
      snd (fullRun [3,9,7,9,10,9,4,9,99,-1,8] [8]) `shouldBe` [0]
      snd (fullRun [3,9,7,9,10,9,4,9,99,-1,8] [7]) `shouldBe` [1]
      snd (fullRun [3,3,1107,-1,8,3,4,3,99] [8]) `shouldBe` [0]
      snd (fullRun [3,3,1107,-1,8,3,4,3,99] [7]) `shouldBe` [1]

    it "isNonZero1" $ do
      snd (fullRun [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]) `shouldBe` [0]
    it "isNonZero2" $ do
      snd (fullRun [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [7]) `shouldBe` [1]
    it "isNonZero3" $ do
      snd (fullRun [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0]) `shouldBe` [0]
    it "isNonZero4" $ do
      snd (fullRun [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [7]) `shouldBe` [1]

    it "compare8" $ do
      let c8 i = snd (fullRun [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [i])
      c8 7 `shouldBe` [999]
      c8 8 `shouldBe` [1000]
      c8 9 `shouldBe` [1001]


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
    