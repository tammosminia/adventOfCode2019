import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day10

main :: IO ()
main = hspec $ do

  describe "day10" $ do
    it "parseInput" $ do
      parseInput ["#.#",".#.","#.#"] `shouldBe` [(0,0), (2,0), (1,1), (0,2), (2,2)]

    it "blocks" $ do
      blocks (0,0) (2,2) (1,1) `shouldBe` True
      blocks (0,0) (2,3) (1,1) `shouldBe` False
      blocks (0,0) (1,1) (2,2) `shouldBe` False
      blocks (0,0) (10,10) (2,2) `shouldBe` True
      blocks (0,0) (8,10) (2,2) `shouldBe` False