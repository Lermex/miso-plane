module Constants where

gameWidth, gameHeight :: Int
(gameWidth, gameHeight) = (480, 480) 

backgroundScrollV :: Double
backgroundScrollV = 0.040

foregroundScrollV :: Double
foregroundScrollV = 0.150

playerX :: Double
playerX = ((fromIntegral gameWidth) / 2 - 150)

jumpSpeed :: Double
jumpSpeed = - 0.370

gravity :: Double
gravity = 0.0015

timeBetweenPillars :: Double
timeBetweenPillars = 1600

pillarWidth :: Int
pillarWidth = 30

minPillarHeight :: Int
minPillarHeight = (gameHeight `div` 8)

planeHeight :: Int
planeHeight = 35

planeWidth :: Int
planeWidth = 60

gapToPlaneRatio :: Double
gapToPlaneRatio = 3.5

gapHeight :: Double
gapHeight = ((fromIntegral planeHeight) * gapToPlaneRatio)

epsilon :: Double
epsilon = 5