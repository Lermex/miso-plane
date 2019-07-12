module Update where

import           Data.Function
import qualified Data.Set      as S
import           Miso
import           System.Random

import           Constants
import           Model

updateModel :: Action -> Model -> Effect Action Model
updateModel action m@Model{..} = case action of
  Time newTime  -> step newTime m
  Keyboard keys -> if S.member 32 keys then noEff (m & transitionState & updatePlayerVelocity) else noEff m
  NewPillars height -> noEff m { pillars = generatePillars height <> pillars }

step :: Double -> Model -> Effect Action Model
step newTime m = batchEff newModel (if shouldAddPillar then [ timeEffect, pillarsEffect ] else [ timeEffect ])
  where
    timeEffect = Time <$> now
    pillarsEffect = NewPillars <$> randomRIO (minPillarHeight, gameHeight - minPillarHeight - round gapHeight)
    shouldAddPillar = timeToPillar newModel == timeBetweenPillars && state newModel == Play
    newModel = m & updateTime newTime
                 & updatePlayerY
                 & updateBackground
                 & applyPhysics
                 & updatePillars
                 & checkFailState
                 & updateScore

updatePillars :: Model -> Model
updatePillars m@Model{..} = m { timeToPillar = newTimeToPillar, pillars = updatedPillars }
  where
    newTimeToPillar =
      if timeToPillar <= 0 then timeBetweenPillars
      else if state == Play then timeToPillar - delta
      else timeToPillar
    updatedPillars = pillars
      & map (\p -> p { pillarX = (pillarX p) - foregroundScrollV * delta })
      & filter (\p -> pillarX p > 0 - fromIntegral pillarWidth)

updateTime :: Double -> Model -> Model
updateTime newTime m@Model{..} = m { time = newTime, delta = newTime - time }

updatePlayerY :: Model -> Model
updatePlayerY m@Model{..} = m { y = newY }
  where
    newY =
      if state == Start then y + (sin (backgroundX / 10))
      else if state == Play || state == GameOver && not (playerOffScreen m) then y + vy * delta
      else y

isColliding :: Model -> Pillar -> Bool
isColliding Model{..} p =
       playerLeft < pillarRight
    && playerRight > pillarLeft
    && playerTop < pillarBottom
    && playerBottom > pillarTop
  where
    playerLeft = playerX + epsilon
    playerTop = y
    playerRight = playerX + fromIntegral planeWidth - epsilon
    playerBottom = y + fromIntegral planeHeight
    pillarLeft = (pillarX p) + epsilon
    pillarTop = (pillarY p)
    pillarRight = (pillarX p) + fromIntegral pillarWidth - epsilon
    pillarBottom = (pillarY p) + fromIntegral (pillarHeight p)

checkFailState :: Model -> Model
checkFailState m@Model{..} = m { state = newState }
  where
    newState = if state == Play && (playerOffScreen m || playerCollidedWithPillar) then GameOver else state
    collisionPillars = filter (isColliding m) pillars & length
    playerCollidedWithPillar = collisionPillars > 0

updateBackground :: Model -> Model
updateBackground m@Model{..} = m { backgroundX = newBackgroundX }
  where
    newBackgroundX =
      if backgroundX > (fromIntegral gameWidth) then 0
      else if state == GameOver then backgroundX
      else backgroundX + (delta * backgroundScrollV)

applyPhysics :: Model -> Model
applyPhysics m@Model{..} = m { vy = newVy }
  where
    newVy = if state == Play || state == GameOver && not (playerOffScreen m) then vy + delta * gravity else 0

generatePillars :: Int -> [Pillar]
generatePillars bottomHeight =
  [ Pillar
    { pillarX = x
    , pillarY = fromIntegral topHeight + gapHeight
    , pillarHeight = bottomHeight
    , pillarKind = Bottom
    , pillarPassed = False
    }
  , Pillar
    { pillarX = x
    , pillarY = 0
    , pillarHeight = topHeight
    , pillarKind = Top
    , pillarPassed = False
    }
  ]
  where
    x = fromIntegral gameWidth
    topHeight = gameHeight - bottomHeight - round gapHeight

updateScore :: Model -> Model
updateScore m@Model{..} = m { pillars = newPillars, score = newScore }
  where
    newlyPassedPillars = pillars & filter (\p -> not (pillarPassed p) && (pillarX p) < playerX) & length
    newPillars = pillars & map markPassedPillar
    newScore = if newlyPassedPillars > 0 then score + 1 else score
    markPassedPillar p = if not (pillarPassed p) && (pillarX p) < playerX then p { pillarPassed = True} else p

transitionState :: Model -> Model
transitionState m@Model{..} =
  if state == GameOver && playerOffScreen m then initialModel
  else m { state = if state == Start then Play else state }

updatePlayerVelocity :: Model -> Model
updatePlayerVelocity m@Model{..} = m { vy = if state == Play then jumpSpeed else vy }

playerOffScreen :: Model -> Bool
playerOffScreen Model{..} = y < 0 || y > fromIntegral gameHeight
