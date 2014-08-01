import Graphics.Gloss.Interface.Pure.Game

import Level.Level

import Play.PlayState
import Play.Events
import Play.Step
import Play.Draw

screenWidth :: Num a => a
screenWidth = 640

screenHeight :: Num a => a
screenHeight = 480

main :: IO ()
main =
    play (InWindow "Pinhole" (screenWidth, screenHeight) (0, 0))
         black
         60
         (playLevel initialLevel)
         draw
         handlePlayEvent
         step
