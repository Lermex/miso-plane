module Main where

import           Miso

import           Model
import           Update
import           View

main :: IO ()
main = do
  initialTime <- now
  startApp App { initialAction = Time initialTime, .. }
    where
      update = updateModel
      model  = initialModel
      view   = mainView
      events = defaultEvents
      subs   = [ keyboardSub Keyboard ]
      mountPoint = Nothing
