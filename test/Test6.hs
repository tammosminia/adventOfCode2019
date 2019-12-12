import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day6
import qualified Data.Map as Map

testOrbitMap = parseOrbits ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN"]

main :: IO ()
main = hspec $ do

  describe "day6" $ do
    it "parseOrbits" $ do
      parseOrbits ["a)b", "b)c1", "b)c2"] `shouldBe` Map.fromList [("b", "a"), ("c1", "b"), ("c2", "b")]

    it "orbitList" $ do
      orbitList testOrbitMap "YOU" `shouldBe` ["K","J","E","D","C","B","COM"]
      orbitList testOrbitMap "SAN" `shouldBe` ["I", "D", "C", "B", "COM"]

    it "distance" $ do
      distance testOrbitMap "YOU" "SAN" `shouldBe` 4


