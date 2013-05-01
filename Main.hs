import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import World
import Collisions

initialWorld :: World
initialWorld = World { balls = [Ball { pos = (0, 240), vel = (0, 0), radius = 20 }]
                     , walls = []
                     , drawing = NotDrawing }

draw :: World -> Picture
draw World { balls = bs
           , walls = ws
           , drawing = dwg } = pictures [ pictures $ map drawBall bs
                                        , pictures $ map drawWall $ case dwg of
                                            Drawing dw -> dw:ws
                                            NotDrawing -> ws ]

drawBall :: Ball -> Picture
drawBall Ball { pos = (x, y), radius = r } =
    Translate x y $ Color white $ Circle r

drawWall :: Wall -> Picture
drawWall (Wall start end) = Color white $ Line [start, end]

handleEvent :: Event -> World -> World
handleEvent event world@(World { drawing = dwg, walls = ws }) =
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
      _ -> world

step :: Float -> World -> World
step dt world@(World { balls = bs, walls = ws }) =
    world { balls = map (\b -> foldr collideWB (stepBall dt b) ws) bs }

gravity :: Float
gravity = 0.03

stepBall :: Float -> Ball -> Ball
stepBall _ ball@(Ball { pos = (x, y), vel = (vx, vy) }) =
    ball { pos = (x + vx, y + vy)
         , vel = (vx, vy - gravity) }

main :: IO ()
main =
    play (InWindow "Pinhole" (640, 480) (0, 0))
         black
         60
         initialWorld
         draw
         handleEvent
         step
