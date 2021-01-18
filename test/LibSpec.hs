module LibSpec where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "lives" $ do
    it "should determine if a cell should stay alive" $ do
      let input = GameState [Point (0,1),Point (1,1),Point (2,1)]
      let expected = True
      lives (Point (1,1)) input `shouldBe` expected
    it "should determine a cell should die" $ do
      let input = GameState [Point (0,1),Point (1,1),Point (2,1)]
      let expected = False
      lives (Point (0,1)) input `shouldBe` expected
    it "should determine if a cell come back to life" $ do
      let input = GameState [Point (0,1),Point (1,1),Point (2,1)]
      let expected = True
      lives (Point (1,0)) input `shouldBe` expected
  describe "tick" $ do
    it "should match pattern 1" $ do
      let input = GameState [Point (0,1),Point (1,1),Point (2,1)]
      let expected = [Point (1,1),Point (1,0),Point (1,2)]
      let (GameState output) = tick input 
      output `shouldMatchList` expected
    it "should match pattern 2" $ do
      let input = GameState [Point (1,1),Point (1,0),Point (1,2)]
      let expected = [Point (0,1),Point (1,1),Point (2,1)]
      let (GameState output) = tick input 
      output `shouldMatchList` expected
