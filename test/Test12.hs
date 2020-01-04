import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Day12


main :: IO ()
main = hspec $ do

  describe "day12" $ do
    it "read Pos" $ do
      let parsed = map (\x -> read x :: Pos) input1s
      (map show parsed) `shouldBe` input1s

    it "gravity" $ do
      gravity Pos {x=1, y=1, z=1} Pos {x=2, y=0, z=0} `shouldBe` Pos {x=1, y=(-1), z=(-1)}

    it "example1" $ do
      energyForSim (steps 10 (initSimulation example1s)) `shouldBe` 179

    it "example2" $ do
      energyForSim (steps 100 (initSimulation example2s)) `shouldBe` 1940

    it "repeatIn example1" $ do
      repeatIn (initSimulation example1s) `shouldBe` 2772

    it "repeatSmart example1" $ do
      repeatSmart (initSimulation example1s) `shouldBe` 2772

    it "repeatSmart example2" $ do
      repeatSmart (initSimulation example2s) `shouldBe` 4686774924

