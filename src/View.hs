module View where

import qualified Data.Map    as M
import           Miso
import           Miso.String

import           Constants
import           Model

mainView :: Model -> View Action
mainView m@Model{..} = wrapper [ div_ [ style_ style, onClick Touched ] content ]
  where
    style = M.fromList
      [ ("width", (ms gameWidth) <> "px")
      , ("height", (ms gameHeight) <> "px")
      , ("overflow", "hidden")
      , ("position", "absolute")
      , ("top", "50%")
      , ("left", "50%")
      , ("transform", "translateX(-240px) translateY(-240px)")
      ]
    content =
      [ backgroundView m
      , playerView m
      , pillarsView m
      , messageView m
      , scoreView m
      ]

backgroundView :: Model -> View action
backgroundView Model{..} = wrapper
  [ image gameWidth gameHeight (negate backgroundX) 0 "images/background.png"
  , image gameWidth gameHeight ((fromIntegral gameWidth) - backgroundX) 0 "images/background.png"
  ]

playerView :: Model -> View action
playerView Model{..} =
  image planeWidth planeHeight playerX y "images/plane.gif"

pillarsView :: Model -> View action
pillarsView m@Model{..} = wrapper $ fmap (pillarView m) pillars

pillarView :: Model -> Pillar -> View action
pillarView Model{..} Pillar{..} =
  let imageName = if pillarKind == Top then "images/topRock.png" else "images/bottomRock.png"
  in image pillarWidth pillarHeight pillarX pillarY imageName

messageView :: Model -> View action
messageView Model{..} = case state of
  GameOver -> image 250 45 115 150 "images/textGameOver.png"
  Start    -> image 250 45 115 150 "images/textGetReady.png"
  _        -> emptyView

scoreView :: Model -> View action
scoreView Model{..} = p_ [ style_ style ] [ text (ms score) ]
  where
    style = M.fromList
      [ ("display", "block")
      , ("height", "50px")
      , ("text-align", "center")
      , ("width", "100%")
      , ("position", "absolute")
      , ("y", "70")
      , ("color", "#32a032")
      , ("font-size", "50px")
      , ("font-weight", "bold")
      , ("font-family", "Helvetica, Arial, sans-serif")
      , ("text-shadow", "-1px 0 #005000, 0 1px #005000, 1px 0 #005000, 0 -1px #005000")
      ]

wrapper :: [View action] -> View action
wrapper = div_ []

emptyView :: View action
emptyView = wrapper []

image :: Int -> Int -> Double -> Double -> MisoString -> View action
image width height offsetX offsetY file = img_
  [ src_ file
  , style_ $ M.fromList
    [ ("display", "block")
    , ("width", (ms width) <> "px")
    , ("height", (ms height) <> "px")
    , ("position", "absolute")
    , ("transform", ms $ "matrix(1,0,0,1," ++ show offsetX ++ ", " ++ show offsetY ++ ")")
    ]
  ]
