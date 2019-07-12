module Model where

import qualified Data.Set    as S
import           Miso
import           Miso.String

import           Constants

data Action
  = Time Double
  | Keyboard (S.Set Int)
  | NewPillars Int
  deriving (Eq, Show)

data State = Play | Start | GameOver
  deriving (Eq, Show)

data PillarKind = Top | Bottom
  deriving (Eq, Show)

data Pillar = Pillar
  { pillarX      :: Double
  , pillarY      :: Double
  , pillarHeight :: Int
  , pillarKind   :: PillarKind
  , pillarPassed :: Bool
  } deriving (Eq, Show)

data Model = Model
  { state        :: State
  , foregroundX  :: Double
  , backgroundX  :: Double
  , y            :: Double
  , vy           :: Double
  , timeToPillar :: Double
  , pillars      :: [Pillar]
  , score        :: Int
  , time         :: Double
  , delta        :: Double
  } deriving (Eq, Show)

initialModel :: Model
initialModel = Model
  { state = Start
  , foregroundX = 0
  , backgroundX = 0
  , y = fromIntegral gameHeight / 2
  , vy = 0
  , timeToPillar = timeBetweenPillars
  , pillars = []
  , score = 0
  , time = 0
  }
