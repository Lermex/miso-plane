module UpdateSpec (spec) where

import Test.Hspec

import Constants
import Model
import Update

-- Helper: a model in Play state with some delta time
playModel :: Model
playModel = initialModel
  { state = Play
  , delta = 100
  , time = 1000
  , vy = 0.1
  }

-- Helper: a model in GameOver state
gameOverModel :: Model
gameOverModel = initialModel
  { state = GameOver
  , delta = 100
  , time = 1000
  , vy = 0.1
  }

-- Helper: a model in GameOver state with player off screen
gameOverOffScreenModel :: Model
gameOverOffScreenModel = gameOverModel { y = -10 }

-- Helper: a pillar directly in front of the player
collidingPillar :: Pillar
collidingPillar = Pillar
  { pillarX = playerX
  , pillarY = 240 - 10
  , pillarHeight = 100
  , pillarKind = Bottom
  , pillarPassed = False
  }

-- Helper: a pillar far away from the player
farPillar :: Pillar
farPillar = Pillar
  { pillarX = 400
  , pillarY = 0
  , pillarHeight = 60
  , pillarKind = Top
  , pillarPassed = False
  }

spec :: Spec
spec = describe "Update" $ do
  describe "playerOffScreen" $ do
    it "returns False when player is in the middle" $
      playerOffScreen initialModel `shouldBe` False

    it "returns True when player is above the screen" $
      playerOffScreen (initialModel { y = -1 }) `shouldBe` True

    it "returns True when player is below the screen" $
      playerOffScreen (initialModel { y = fromIntegral gameHeight + 1 }) `shouldBe` True

    it "returns False at y = 0 (top boundary)" $
      playerOffScreen (initialModel { y = 0 }) `shouldBe` False

    it "returns False at y = gameHeight (bottom boundary)" $
      playerOffScreen (initialModel { y = fromIntegral gameHeight }) `shouldBe` False

    it "returns True just above top boundary" $
      playerOffScreen (initialModel { y = -0.001 }) `shouldBe` True

    it "returns True just below bottom boundary" $
      playerOffScreen (initialModel { y = fromIntegral gameHeight + 0.001 }) `shouldBe` True

  describe "isColliding" $ do
    it "detects collision when pillar overlaps player" $
      isColliding playModel collidingPillar `shouldBe` True

    it "returns False when pillar is far away" $
      isColliding playModel farPillar `shouldBe` False

    it "returns False when pillar is to the right of player" $ do
      let p = collidingPillar { pillarX = playerX + fromIntegral planeWidth + 10 }
      isColliding playModel p `shouldBe` False

    it "returns False when pillar is to the left of player" $ do
      let p = collidingPillar { pillarX = playerX - fromIntegral pillarWidth - 10 }
      isColliding playModel p `shouldBe` False

    it "returns False when pillar is above player" $ do
      let p = collidingPillar { pillarY = 0, pillarHeight = round (y playModel) - 10 }
      isColliding playModel p `shouldBe` False

    it "returns False when pillar is below player" $ do
      let p = collidingPillar { pillarY = y playModel + fromIntegral planeHeight + 10 }
      isColliding playModel p `shouldBe` False

    it "accounts for epsilon tolerance" $ do
      -- Pillar just barely touching (within epsilon) should not collide
      let p = collidingPillar { pillarX = playerX + fromIntegral planeWidth - epsilon }
      isColliding playModel p `shouldBe` False

  describe "generatePillars" $ do
    it "generates exactly 2 pillars" $
      length (generatePillars 100) `shouldBe` 2

    it "generates one Bottom and one Top pillar" $ do
      let ps = generatePillars 100
      length (filter (\p -> pillarKind p == Bottom) ps) `shouldBe` 1
      length (filter (\p -> pillarKind p == Top) ps) `shouldBe` 1

    it "places pillars at x = gameWidth" $ do
      let ps = generatePillars 100
      all (\p -> pillarX p == fromIntegral gameWidth) ps `shouldBe` True

    it "pillars are not initially passed" $ do
      let ps = generatePillars 100
      all (\p -> not (pillarPassed p)) ps `shouldBe` True

    it "bottom pillar has given height" $ do
      let ps = generatePillars 150
      let bottom = head $ filter (\p -> pillarKind p == Bottom) ps
      pillarHeight bottom `shouldBe` 150

    it "top pillar height is gameHeight - bottomHeight - gapHeight" $ do
      let bottomH = 150
      let ps = generatePillars bottomH
      let top = head $ filter (\p -> pillarKind p == Top) ps
      pillarHeight top `shouldBe` (gameHeight - bottomH - round gapHeight)

    it "top pillar starts at y = 0" $ do
      let ps = generatePillars 100
      let top = head $ filter (\p -> pillarKind p == Top) ps
      pillarY top `shouldBe` 0

    it "bottom pillar Y = topHeight + gapHeight" $ do
      let bottomH = 100
      let ps = generatePillars bottomH
      let bottom = head $ filter (\p -> pillarKind p == Bottom) ps
      let topH = gameHeight - bottomH - round gapHeight
      pillarY bottom `shouldBe` (fromIntegral topH + gapHeight)

  describe "updatePlayerY" $ do
    it "applies sine wave bobbing in Start state" $ do
      let m = initialModel { backgroundX = 50 }
      let m' = updatePlayerY m
      y m' `shouldBe` (y initialModel + sin (50 / 10))

    it "applies velocity in Play state" $ do
      let m = playModel { y = 200, vy = 0.2, delta = 100 }
      let m' = updatePlayerY m
      y m' `shouldBe` (200 + 0.2 * 100)

    it "applies velocity in GameOver state when on screen" $ do
      let m = gameOverModel { y = 200, vy = 0.2, delta = 100 }
      let m' = updatePlayerY m
      y m' `shouldBe` (200 + 0.2 * 100)

    it "does not change Y in GameOver state when off screen" $ do
      let m = gameOverOffScreenModel
      let m' = updatePlayerY m
      y m' `shouldBe` y m

  describe "applyPhysics" $ do
    it "increases velocity by gravity in Play state" $ do
      let m = playModel { vy = 0.1, delta = 100 }
      let m' = applyPhysics m
      vy m' `shouldBe` (0.1 + 100 * gravity)

    it "increases velocity by gravity in GameOver state (on screen)" $ do
      let m = gameOverModel { vy = 0.1, delta = 100, y = 200 }
      let m' = applyPhysics m
      vy m' `shouldBe` (0.1 + 100 * gravity)

    it "resets velocity to 0 in Start state" $ do
      let m = initialModel { vy = 5 }
      let m' = applyPhysics m
      vy m' `shouldBe` 0

    it "resets velocity to 0 in GameOver state when off screen" $ do
      let m = gameOverOffScreenModel { vy = 5 }
      let m' = applyPhysics m
      vy m' `shouldBe` 0

  describe "updateBackground" $ do
    it "scrolls background in Play state" $ do
      let m = playModel { backgroundX = 100, delta = 100 }
      let m' = updateBackground m
      backgroundX m' `shouldBe` (100 + 100 * backgroundScrollV)

    it "scrolls background in Start state" $ do
      let m = initialModel { backgroundX = 100, delta = 100 }
      let m' = updateBackground m
      backgroundX m' `shouldBe` (100 + 100 * backgroundScrollV)

    it "wraps background when exceeding gameWidth" $ do
      let m = playModel { backgroundX = fromIntegral gameWidth + 1 }
      let m' = updateBackground m
      backgroundX m' `shouldBe` 0

    it "freezes background in GameOver state" $ do
      let m = gameOverModel { backgroundX = 100, delta = 100 }
      let m' = updateBackground m
      backgroundX m' `shouldBe` 100

  describe "updateTime" $ do
    it "sets time to new time" $ do
      let m = initialModel { time = 1000 }
      let m' = updateTime 1500 m
      time m' `shouldBe` 1500

    it "calculates delta correctly" $ do
      let m = initialModel { time = 1000 }
      let m' = updateTime 1500 m
      delta m' `shouldBe` 500

    it "handles first update from zero" $ do
      let m' = updateTime 100 initialModel
      time m' `shouldBe` 100
      delta m' `shouldBe` 100

  describe "updateScore" $ do
    it "increments score when player passes a pillar" $ do
      let passedPillar = farPillar { pillarX = playerX - 10, pillarPassed = False }
      let m = playModel { pillars = [passedPillar], score = 5 }
      let m' = updateScore m
      score m' `shouldBe` 6

    it "does not increment score for already-passed pillars" $ do
      let passedPillar = farPillar { pillarX = playerX - 10, pillarPassed = True }
      let m = playModel { pillars = [passedPillar], score = 5 }
      let m' = updateScore m
      score m' `shouldBe` 5

    it "does not increment score for pillars ahead of player" $ do
      let m = playModel { pillars = [farPillar], score = 5 }
      let m' = updateScore m
      score m' `shouldBe` 5

    it "marks passed pillars as passed" $ do
      let passedPillar = farPillar { pillarX = playerX - 10, pillarPassed = False }
      let m = playModel { pillars = [passedPillar] }
      let m' = updateScore m
      all pillarPassed (pillars m') `shouldBe` True

    it "increments score by 1 even when both top and bottom pass" $ do
      -- When both pillars of a pair pass, score increments by 1 (not 2)
      -- because updateScore counts newlyPassedPillars > 0, not the count
      let p1 = farPillar { pillarX = playerX - 10, pillarPassed = False, pillarKind = Top }
      let p2 = farPillar { pillarX = playerX - 10, pillarPassed = False, pillarKind = Bottom }
      let m = playModel { pillars = [p1, p2], score = 0 }
      let m' = updateScore m
      score m' `shouldBe` 1

  describe "transitionState" $ do
    it "transitions Start to Play" $ do
      let m' = transitionState initialModel
      state m' `shouldBe` Play

    it "keeps Play as Play" $ do
      let m' = transitionState playModel
      state m' `shouldBe` Play

    it "keeps GameOver when player is on screen" $ do
      let m = gameOverModel { y = 200 }
      let m' = transitionState m
      state m' `shouldBe` GameOver

    it "resets to initialModel when GameOver and player is off screen" $ do
      let m' = transitionState gameOverOffScreenModel
      m' `shouldBe` initialModel

  describe "updatePlayerVelocity" $ do
    it "sets velocity to jumpSpeed in Play state" $ do
      let m' = updatePlayerVelocity playModel
      vy m' `shouldBe` jumpSpeed

    it "does not change velocity in Start state" $ do
      let m = initialModel { vy = 0.5 }
      let m' = updatePlayerVelocity m
      vy m' `shouldBe` 0.5

    it "does not change velocity in GameOver state" $ do
      let m = gameOverModel { vy = 0.5 }
      let m' = updatePlayerVelocity m
      vy m' `shouldBe` 0.5

  describe "jump" $ do
    it "transitions Start to Play and sets jumpSpeed" $ do
      let m' = jump initialModel
      state m' `shouldBe` Play
      vy m' `shouldBe` jumpSpeed

    it "sets jumpSpeed in Play state" $ do
      let m' = jump playModel
      state m' `shouldBe` Play
      vy m' `shouldBe` jumpSpeed

    it "does not change GameOver state when on screen" $ do
      let m = gameOverModel { y = 200 }
      let m' = jump m
      state m' `shouldBe` GameOver

    it "resets game when GameOver and off screen" $ do
      let m' = jump gameOverOffScreenModel
      -- transitionState resets to initialModel, then updatePlayerVelocity
      -- Since initialModel is Start state, velocity is not changed by updatePlayerVelocity
      state m' `shouldBe` Start
      vy m' `shouldBe` 0

  describe "checkFailState" $ do
    it "transitions to GameOver on collision" $ do
      let m = playModel { y = 240, pillars = [collidingPillar] }
      let m' = checkFailState m
      state m' `shouldBe` GameOver

    it "transitions to GameOver when player is off screen" $ do
      let m = playModel { y = -10 }
      let m' = checkFailState m
      state m' `shouldBe` GameOver

    it "keeps Play when no collision and on screen" $ do
      let m = playModel { y = 200, pillars = [farPillar] }
      let m' = checkFailState m
      state m' `shouldBe` Play

    it "keeps Play with no pillars" $ do
      let m = playModel { y = 200, pillars = [] }
      let m' = checkFailState m
      state m' `shouldBe` Play

    it "does not change Start state" $ do
      let m' = checkFailState initialModel
      state m' `shouldBe` Start

    it "does not change GameOver state" $ do
      let m' = checkFailState gameOverModel
      state m' `shouldBe` GameOver

  describe "updatePillars" $ do
    it "moves pillars left" $ do
      let p = farPillar { pillarX = 300 }
      let m = playModel { pillars = [p], delta = 100 }
      let m' = updatePillars m
      let [p'] = pillars m'
      pillarX p' `shouldBe` (300 - foregroundScrollV * 100)

    it "removes off-screen pillars" $ do
      let p = farPillar { pillarX = -fromIntegral pillarWidth - 1 }
      let m = playModel { pillars = [p], delta = 0 }
      let m' = updatePillars m
      pillars m' `shouldBe` []

    it "keeps pillars that are still visible" $ do
      let p = farPillar { pillarX = 100 }
      let m = playModel { pillars = [p], delta = 0 }
      let m' = updatePillars m
      length (pillars m') `shouldBe` 1

    it "decrements timeToPillar in Play state" $ do
      let m = playModel { timeToPillar = 1000, delta = 100 }
      let m' = updatePillars m
      timeToPillar m' `shouldBe` 900

    it "resets timeToPillar when it reaches zero" $ do
      let m = playModel { timeToPillar = 0, delta = 100 }
      let m' = updatePillars m
      timeToPillar m' `shouldBe` timeBetweenPillars

    it "does not decrement timeToPillar in Start state" $ do
      let m = initialModel { timeToPillar = 1000, delta = 100 }
      let m' = updatePillars m
      timeToPillar m' `shouldBe` 1000

    it "does not decrement timeToPillar in GameOver state" $ do
      let m = gameOverModel { timeToPillar = 1000, delta = 100 }
      let m' = updatePillars m
      timeToPillar m' `shouldBe` 1000
