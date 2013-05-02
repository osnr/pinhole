import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

import World
import Collisions

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
                     , walls = [ Wall (-screenWidth/2, -screenHeight/2 + 1) (-screenWidth/3, -screenHeight/2 + 1) -- bottom-left
                               , Wall (-screenWidth/4, -screenHeight/2 + 1) (screenWidth/2, -screenHeight/2 + 1) -- bottom-right
                               , Wall (-screenWidth/2 + 1, -screenHeight/2) (-screenWidth/2 + 1, screenHeight/2) -- left
                               , Wall (screenWidth/2 - 1, -screenHeight/2) (screenWidth/2 - 1, screenHeight/2) -- right
                               ]
                     , drawing = NotDrawing
                     , future = False }

drawPlay :: World -> Picture
drawPlay world | future world = pictures [ drawFutures world, drawWorld world ]
               | otherwise = drawWorld world

drawWorld :: World -> Picture
drawWorld World { balls = bs, walls = ws, drawing = dwg } =
    pictures [ pictures $ map (drawBall white 2) bs
             , pictures $ map drawWall $ case dwg of
                                           Drawing dw -> dw:ws
                                           NotDrawing -> ws
             ]

drawFutureBalls :: Float -> (Int, World) -> Picture
drawFutureBalls maxN (n, World { balls = bs }) = 
    let com = 1 - logBase maxN (fromIntegral n)
        clr = makeColor com com com com in
    pictures $ map (drawBall clr com) bs

drawFutures :: World -> Picture
drawFutures = pictures . take 20 . map (drawFutureBalls 50000) .
              filter ((== 0) . (`mod` 15) . fst) . zip [1..] . iterate (stepPlay 0)

drawBall :: Color -> Float -> Ball -> Picture
drawBall clr th Ball { pos = (x, y), theta = t, radius = r } =
    let (d1x, d1y) = rotateV t (2*r, 0)
        (d2x, d2y) = rotateV t (0, 2*r) in
    pictures [ Color clr $ Pictures [ Translate x y $ ThickCircle r th
                                    ]] --, Line [(x - d1x/2, y - d1y/2), (x + d1x/2, y + d1y/2)]
                                    --, Line [(x - d2x/2, y - d2y/2), (x + d2x/2, y + d2y/2)] ] ]
             -- , Color green $ Line [(x, y), (x + vx*5, y + vy*5)] ]

drawWall :: Wall -> Picture
drawWall (Wall start end) = Color white $ Line [start, end]

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
      _ -> world

stepPlay :: Float -> World -> World
stepPlay dt world@(World { balls = bs, walls = ws, drawing = dwg }) =
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
