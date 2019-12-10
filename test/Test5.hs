import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day5

main :: IO ()
main = hspec $ do
  describe "day5" $ do
    it "parseOperation" $ do
      parseOperation 1 `shouldBe` (1, [False, False], 1)
      parseOperation 1002 `shouldBe` (2, [False, True], 1)
      parseOperation 1099 `shouldBe` (99, [], 0)

    it "readInputParameters" $ do
      readInputParameters [True, False] 1 [1, 50, 0] `shouldBe` [50, 1]

    it "fullRun" $ do
      fullRun [1002,4,3,4,33] [] `shouldBe` ([1002,4,3,4,99], [])
      fullRun [1103,0,4,0,99] [13] `shouldBe` ([13,0,4,0,99], [13])

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

    it "isNonZero" $ do
      snd (fullRun [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]) `shouldBe` [0]
      snd (fullRun [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [7]) `shouldBe` [1]
      snd (fullRun [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0]) `shouldBe` [0]
      snd (fullRun [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [7]) `shouldBe` [1]

    it "compare8" $ do
      let c8 i = snd (fullRun [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [i])
      c8 7 `shouldBe` [999]
      c8 8 `shouldBe` [1000]
      c8 9 `shouldBe` [1001]






