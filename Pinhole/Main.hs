import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

import Play.Draw
import Play.Step
import Play.World

import Debug.Trace

data GameState = Intro Float | IntroLevel Float World | PlayLevel World

screenWidth :: Num a => a
screenWidth = 640

screenHeight :: Num a => a
screenHeight = 480

initialWorld :: World
initialWorld = World { balls = [Ball { pos = (0, 240)
                                     , vel = (0, 0)
                                     , theta = 0 -- radians
                                     , omega = 0
                                     , radius = 20 }]
                     , walls = [ Wall (-screenWidth/2, -screenHeight/2 + 1)
                                          (-screenWidth/3, -screenHeight/2 + 1) -- bottom-left
                               , Wall (-screenWidth/4, -screenHeight/2 + 1)
                                          (screenWidth/2, -screenHeight/2 + 1) -- bottom-right
                               , Wall (-screenWidth/2 + 1, -screenHeight/2)
                                          (-screenWidth/2 + 1, screenHeight/2) -- left
                               , Wall (screenWidth/2 - 1, -screenHeight/2)
                                          (screenWidth/2 - 1, screenHeight/2) -- right
                               ]
                     , drawing = NotDrawing
                     , future = False }

handlePlayEvent :: Event -> World -> World
handlePlayEvent event world@(World { drawing = dwg, walls = ws, future = ft }) =
    case event of
      EventKey (MouseButton LeftButton) Down _ mPos ->
          case dwg of
            NotDrawing -> world { drawing = Drawing $ Wall mPos mPos }
            Drawing _ -> world
      EventKey (MouseButton LeftButton) Up _ _ ->
          case dwg of
            NotDrawing -> world
            Drawing w@(Wall start end) ->
                world {
                     walls = if (end - start) /= 0
                             then w:ws
                             else ws
                   , drawing = NotDrawing }
      EventMotion mPos ->
          case dwg of
            NotDrawing -> world
            Drawing (Wall start _) -> world { drawing = Drawing $ Wall start mPos }
      EventKey (Char 'f') Up _ _ ->
          world { future = not ft }
      EventKey (Char 'r') Up _ _ ->
          initialWorld
      _ -> world

draw :: GameState -> Picture
draw (Intro _) = Translate (-160) 0 . Color white . Scale 0.5 0.5 $ Text "Pinhole"
draw (IntroLevel _ _) = Text "Level 1"
draw (PlayLevel w) = drawPlay w

handleEvent :: Event -> GameState -> GameState
handleEvent _ (Intro t) = Intro t
handleEvent _ (IntroLevel t w) = IntroLevel t w
handleEvent e (PlayLevel w) = PlayLevel $ handlePlayEvent e w 

step :: Float -> GameState -> GameState
step dt (Intro t) | t > 5 = PlayLevel initialWorld
                  | otherwise = Intro (t + dt)
step dt (IntroLevel t w) = IntroLevel (t + dt) w
step dt (PlayLevel w) = PlayLevel $ stepPlay dt w

main :: IO ()
main =
    play (InWindow "Pinhole" (screenWidth, screenHeight) (0, 0))
         black
         60
         (PlayLevel initialWorld)
         draw
         handleEvent
         step
