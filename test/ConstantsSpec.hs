module ConstantsSpec (spec) where

import Test.Hspec

import Constants

spec :: Spec
spec = describe "Constants" $ do
  describe "game dimensions" $ do
    it "gameWidth is 480" $
      gameWidth `shouldBe` 480

    it "gameHeight is 480" $
      gameHeight `shouldBe` 480

  describe "scroll velocities" $ do
    it "backgroundScrollV is 0.040" $
      backgroundScrollV `shouldBe` 0.040

    it "foregroundScrollV is 0.150" $
      foregroundScrollV `shouldBe` 0.150

    it "foreground scrolls faster than background" $
      foregroundScrollV `shouldSatisfy` (> backgroundScrollV)

  describe "player constants" $ do
    it "playerX is centered minus 150" $
      playerX `shouldBe` (fromIntegral gameWidth / 2 - 150)

    it "playerX equals 90" $
      playerX `shouldBe` 90

    it "planeHeight is 35" $
      planeHeight `shouldBe` 35

    it "planeWidth is 60" $
      planeWidth `shouldBe` 60

  describe "physics constants" $ do
    it "jumpSpeed is negative (upward)" $
      jumpSpeed `shouldSatisfy` (< 0)

    it "gravity is positive (downward)" $
      gravity `shouldSatisfy` (> 0)

  describe "pillar constants" $ do
    it "pillarWidth is 30" $
      pillarWidth `shouldBe` 30

    it "minPillarHeight is gameHeight / 8" $
      minPillarHeight `shouldBe` (gameHeight `div` 8)

    it "minPillarHeight equals 60" $
      minPillarHeight `shouldBe` 60

    it "timeBetweenPillars is 1600" $
      timeBetweenPillars `shouldBe` 1600

  describe "gap constants" $ do
    it "gapToPlaneRatio is 3.5" $
      gapToPlaneRatio `shouldBe` 3.5

    it "gapHeight equals planeHeight * gapToPlaneRatio" $
      gapHeight `shouldBe` (fromIntegral planeHeight * gapToPlaneRatio)

    it "gapHeight equals 122.5" $
      gapHeight `shouldBe` 122.5

  describe "epsilon" $ do
    it "epsilon is 5" $
      epsilon `shouldBe` 5

    it "epsilon is positive" $
      epsilon `shouldSatisfy` (> 0)
