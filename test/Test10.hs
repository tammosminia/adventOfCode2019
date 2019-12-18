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
  
--    it "example2" $ do
--      bestAsteroid ["......#.#.","#..#.#....","..#######.",".#.#.###..",".#..#.....","..#....#.#","#..#....#.",".##.#..###","##...#..#.",".#....####"] `shouldBe` (33,(5,8))
  
      