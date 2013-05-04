module Play.Draw
( drawPlay
, drawWorld
, drawFutureBalls
, drawFutures
, drawBall
, drawWall ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Play.World
import Play.Step

drawPlay :: World -> Picture
drawPlay world | future world = Pictures [ drawFutures world, drawWorld world ]
               | otherwise = drawWorld world

drawWorld :: World -> Picture
drawWorld World { balls = bs, walls = ws, drawing = dwg } =
    Pictures [ Pictures $ map (drawBall white 2) bs
             , Pictures $ map drawWall $ case dwg of
                                           Drawing dw -> dw:ws
                                           NotDrawing -> ws
             ]

drawFutureBalls :: Float -> (Int, World) -> Picture
drawFutureBalls maxN (n, World { balls = bs }) = 
    let com = 1 - logBase maxN (fromIntegral n)
        clr = makeColor com com com com in
    pictures $ map (drawBall clr com) bs

drawFutures :: World -> Picture
drawFutures = Pictures . take 40 . map (drawFutureBalls 50000) .
              filter ((== 0) . (`mod` 15) . fst) . zip [1..] . iterate (stepPlay 0)

drawBall :: Color -> Float -> Ball -> Picture
drawBall clr th Ball { pos = (x, y), theta = t, radius = r, vel = (vx, vy) } =
    let (d1x, d1y) = rotateV t (2*r, 0)
        (d2x, d2y) = rotateV t (0, 2*r) in
    pictures [ Color clr $ Pictures [ Translate x y $ ThickCircle r th
                                    , Line [(x - d1x/2, y - d1y/2), (x + d1x/2, y + d1y/2)]
                                    , Line [(x - d2x/2, y - d2y/2), (x + d2x/2, y + d2y/2)] ]
             , Color green $ Line [(x, y), (x + vx*5, y + vy*5)] ]

drawWall :: Wall -> Picture
drawWall (Wall start end) = Color white $ Line [start, end]
