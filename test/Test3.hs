import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day3

main :: IO ()
main = hspec $ do
  describe "day3" $ do
    it "crosses" $ do
      crosses inputA (100, 0) `shouldBe` True
      crosses inputA (1005, 500) `shouldBe` True
      
    it "wireDistance" $ do
      wireDistance (100, 0) inputA `shouldBe` 100
      wireDistance (1005, 500) inputA `shouldBe` 1505
