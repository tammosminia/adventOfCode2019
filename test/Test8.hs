import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day8

main :: IO ()
main = hspec $ do

  describe "day8" $ do
    it "layerCounts" $ do
      countLayer "120120120" `shouldBe` [3, 3, 3]
      countLayer "120121" `shouldBe` [1, 3, 2]
      
    it "render" $ do
      render (splitEvery 4 "0222112222120000") `shouldBe` "0110"
  