import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

import World
import Collisions

initialWorld :: World
initialWorld = World { balls = [Ball { pos = (0, 240)
                                     , vel = (0, 0)
                                     , theta = 0 -- radians
                                     , omega = 0
                                     , radius = 20 }]
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
drawBall Ball { pos = (x, y), vel = (vx, vy), theta = t, radius = r } =
    let (d1x, d1y) = rotateV t (2*r, 0)
        (d2x, d2y) = rotateV t (0, 2*r) in
    pictures [ Color white $ Pictures [ Translate x y $ Circle r
                                      , Line [(x - d1x/2, y - d1y/2), (x + d1x/2, y + d1y/2)]
                                      , Line [(x - d2x/2, y - d2y/2), (x + d2x/2, y + d2y/2)] ]
             , Color green $ Line [(x, y), (x + vx*5, y + vy*5)] ]

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
stepBall _ ball@(Ball { pos = (x, y), vel = (vx, vy), theta = t, omega = w }) =
    ball { pos = (x + vx, y + vy)
         , vel = (vx, vy - gravity)
         , theta = t + w }

main :: IO ()
main =
    play (InWindow "Pinhole" (640, 480) (0, 0))
         black
         60
         initialWorld
         draw
         handleEvent
         step
