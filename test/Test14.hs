import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day14

import qualified Data.Set as Set

main :: IO ()
main = hspec $ do

  describe "day14" $ do
    it "only one synth per chemical" $ do
      let synthables = [ c | (_, (a, c)) <- input1 ]
      (length $ Set.fromList synthables) `shouldBe` length synthables

    it "example1 desynth" $ do
      desynth [(1, "FUEL")] example1 `shouldBe` [(-2,"A"),(31,"ORE")]

    it "example1" $ do
      makeFuel example1 `shouldBe` 31

    it "example2" $ do
      makeFuel example2 `shouldBe` 165

    it "example3" $ do
      makeFuel example3 `shouldBe` 13312

    it "example4" $ do
      makeFuel example4 `shouldBe` 180697

    it "example5" $ do
      makeFuel example5 `shouldBe` 2210736

    it "example3-2" $ do
      trillionOre example3 `shouldBe` 82892753

    it "example4-2" $ do
      trillionOre example4 `shouldBe` 5586022

    it "example5-2" $ do
      trillionOre example5 `shouldBe` 460664



