module ModelSpec (spec) where

import Test.Hspec

import Constants
import Model

spec :: Spec
spec = describe "Model" $ do
  describe "initialModel" $ do
    it "starts in Start state" $
      state initialModel `shouldBe` Start

    it "player Y is at vertical center" $
      y initialModel `shouldBe` fromIntegral gameHeight / 2

    it "player Y equals 240" $
      y initialModel `shouldBe` 240

    it "initial velocity is zero" $
      vy initialModel `shouldBe` 0

    it "initial score is zero" $
      score initialModel `shouldBe` 0

    it "has no pillars" $
      pillars initialModel `shouldBe` []

    it "foregroundX starts at 0" $
      foregroundX initialModel `shouldBe` 0

    it "backgroundX starts at 0" $
      backgroundX initialModel `shouldBe` 0

    it "time starts at 0" $
      time initialModel `shouldBe` 0

    it "delta starts at 0" $
      delta initialModel `shouldBe` 0

    it "timeToPillar starts at timeBetweenPillars" $
      timeToPillar initialModel `shouldBe` timeBetweenPillars

  describe "State" $ do
    it "Play, Start, and GameOver are distinct" $ do
      Play `shouldNotBe` Start
      Play `shouldNotBe` GameOver
      Start `shouldNotBe` GameOver

  describe "PillarKind" $ do
    it "Top and Bottom are distinct" $
      Top `shouldNotBe` Bottom

  describe "Pillar equality" $ do
    it "identical pillars are equal" $ do
      let p = Pillar 100 200 60 Top False
      p `shouldBe` p

    it "pillars with different positions are not equal" $ do
      let p1 = Pillar 100 200 60 Top False
      let p2 = Pillar 150 200 60 Top False
      p1 `shouldNotBe` p2
