import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day4

main :: IO ()
main = hspec $ do
  describe "day4" $ do
    it "sameAdjacent 111111" $ do
      let d = (toDigits 111111) 
        in (sameAdjacent d) `shouldBe` True

    it "sameAdjacent 123789" $ do
      let d = (toDigits 123789) 
        in (sameAdjacent d) `shouldBe` False
    
    it "check" $ do
      checkNumber 111111 `shouldBe` True
      checkNumber 223450 `shouldBe` False
      checkNumber 123789 `shouldBe` False

    it "split" $ do
      split (toDigits 122233) `shouldBe` [[1], [2,2,2], [3,3]]
      
    it "doubleAdjacent" $ do
      doubleAdjacent (toDigits 122233) `shouldBe` True
      doubleAdjacent (toDigits 123456) `shouldBe` False
