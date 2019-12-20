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
      blocks (0,0) (1,1) (2,2) `shouldBe` True
      blocks (0,0) (1,1) (2,3) `shouldBe` False
      blocks (0,0) (2,2) (1,1) `shouldBe` False
      blocks (0,0) (2,2) (10,10) `shouldBe` True
      blocks (0,0) (2,2) (8,10) `shouldBe` False
      blocks (0,0) (0,0) (2,2) `shouldBe` False
      blocks (0,0) (2,2) (0,0) `shouldBe` False
      blocks (0,0) (0,1) (0,2) `shouldBe` True
      blocks (4,0) (4,2) (4,4) `shouldBe` True
      blocks (4,0) (4,3) (4,4) `shouldBe` True

    it "example1" $ do
      bestAsteroid example1 `shouldBe` (8,(3,4))
  
    it "example2" $ do
      bestAsteroid example2 `shouldBe` (33,(5,8))

    it "example3" $ do
      bestAsteroid example3 `shouldBe` (35,(1,2))

    it "example4" $ do
      bestAsteroid example4 `shouldBe` (41,(6,3))

--    it "example5" $ do
--      bestAsteroid example5 `shouldBe` (210,(11,13))

    it "sortClockwise" $ do
      sortClockwise [(1,1),(2,1),(1,2),(1,3),(3,1),(10,9)] `shouldBe` [(3,1),(2,1),(10,9),(1,1),(1,2),(1,3)]
      sortClockwise [(1,1),(-1,1),(1,-1),(-1,-1),(0,-1),(0,1),(-1,0),(1,0)] `shouldBe` [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

    it "example 2-1" $ do
      take 2 (vaporize (8,3) (parseInput example21)) `shouldBe` [(8,1),(9,0)]

