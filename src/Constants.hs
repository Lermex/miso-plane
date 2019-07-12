module Constants where

(gameWidth,gameHeight) = (480, 480) :: (Int, Int)
backgroundScrollV = 0.040 :: Double
foregroundScrollV = 0.150 :: Double
playerX = ((fromIntegral gameWidth) / 2 - 150) :: Double
jumpSpeed = - 0.370  :: Double
gravity = 0.0015 :: Double
timeBetweenPillars = 1600 :: Double
pillarWidth = 30 :: Int
minPillarHeight = (gameHeight `div` 8) :: Int
planeHeight = 35 :: Int
planeWidth = 60 :: Int
gapToPlaneRatio = 3.5 :: Double
gapHeight = ((fromIntegral planeHeight) * gapToPlaneRatio) :: Double
epsilon = 5 :: Double