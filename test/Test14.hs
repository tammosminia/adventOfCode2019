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

