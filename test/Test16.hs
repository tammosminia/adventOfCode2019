import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day16

main :: IO ()
main = hspec $ do

  describe "day16" $ do
    it "lastDigit" $ do
      lastDigit 32 `shouldBe` 2
      lastDigit (-17) `shouldBe` 7

    it "toInt" $ do
      toInt [1,2,3,4,5,6,7,8] `shouldBe` 12345678
      toInt [4,8,2,2,6,1,5,8] `shouldBe` 48226158

    it "pattern" $ do
      take 8 (pattern 1) `shouldBe` [1,0,-1,0,1,0,-1,0]
      take 8 (pattern 2) `shouldBe` [0,1,1,0,0,-1,-1,0]
      take 8 (pattern 3) `shouldBe` [0,0,1,1,1,0,0,0]
      take 8 (pattern 4) `shouldBe` [0,0,0,1,1,1,1,0]

    it "phase" $ do
      phase [1,2,3,4,5,6,7,8] `shouldBe` [4,8,2,2,6,1,5,8]

    it "phases" $ do
      phasesI 1 12345678 `shouldBe` 48226158
      phasesI 2 12345678 `shouldBe` 34040438
      phasesI 3 12345678 `shouldBe` 03415518
      phasesI 4 12345678 `shouldBe` 01029498

    it "phases large" $ do
      let lphase x = toInt $ take 8 $ toDigits $ phasesI 100 x
      lphase 80871224585914546619083218645595 `shouldBe` 24176176
      lphase 19617804207202209144916044189917 `shouldBe` 73745418
      lphase 69317163492948606335995924319873 `shouldBe` 52432133


