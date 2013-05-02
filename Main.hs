import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

import World
import Collisions

import Debug.Trace

initialWorld :: World
initialWorld = World { balls = [Ball { pos = (0, 240)
                                     , vel = (0, 0)
                                     , theta = 0 -- radians
                                     , omega = 0
                                     , radius = 20 }]
                     , walls = []
                     , drawing = NotDrawing }

draw :: World -> Picture
draw world = pictures [ drawFutures world, drawWorld world ]

drawWorld :: World -> Picture
drawWorld World { balls = bs, walls = ws, drawing = dwg } =
    pictures [ pictures $ map (drawBall white 2) bs
             , pictures $ map drawWall $ case dwg of
                                           Drawing dw -> dw:ws
                                           NotDrawing -> ws
             ]

everyNth :: Int -> [a] -> [a]
everyNth n xs = y:everyNth n ys where y:ys = drop (n-1) xs 

drawFutureBalls :: Float -> (Int, World) -> Picture
drawFutureBalls maxN (n, World { balls = bs }) = 
    let com = 1 - fromIntegral n / maxN
        clr = makeColor com com com com in
    pictures $ map (drawBall clr com) bs

drawFutures :: World -> Picture
drawFutures = pictures . take 10 . map (drawFutureBalls 180) .
              filter ((== 0) . (`mod` 15) . fst) . zip [1..] . iterate (step 0)

drawBall :: Color -> Float -> Ball -> Picture
drawBall clr th Ball { pos = (x, y), vel = (vx, vy), theta = t, radius = r } =
    let (d1x, d1y) = rotateV t (2*r, 0)
        (d2x, d2y) = rotateV t (0, 2*r) in
    pictures [ Color clr $ Pictures [ Translate x y $ ThickCircle r th
                                    , Line [(x - d1x/2, y - d1y/2), (x + d1x/2, y + d1y/2)]
                                    , Line [(x - d2x/2, y - d2y/2), (x + d2x/2, y + d2y/2)] ] ]
             -- , Color green $ Line [(x, y), (x + vx*5, y + vy*5)] ]

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
step dt world@(World { balls = bs, walls = ws, drawing = dwg }) =
    world { balls = let ws' = case dwg of
                                Drawing dw -> dw:ws
                                NotDrawing -> ws in
                    map (\b -> foldr collideWB (stepBall dt b) ws') bs }

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
