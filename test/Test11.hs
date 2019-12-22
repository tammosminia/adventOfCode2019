import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day11


main :: IO ()
main = hspec $ do

  describe "day11" $ do
    it "stepRobot first steps" $ do
      let (prog10,o0) = stepRobot prog1 0
      o0 `shouldBe` [1,0]
      let (prog11,o1) = stepRobot prog10 0
      o1 `shouldBe` [1,1]

