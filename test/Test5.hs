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

