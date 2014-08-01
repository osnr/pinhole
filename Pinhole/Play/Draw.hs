module Play.Draw
( drawPlay
, drawPlayState
, drawBall
, drawWall ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Color

import Level.Level

import Play.PlayState
import Play.Step

drawPlay :: PlayState -> Picture
drawPlay pl | future pl = pictures [ drawFutures pl, drawPlayState pl ]
            | otherwise = drawPlayState pl

drawPlayState :: PlayState -> Picture
drawPlayState PlayState { level = l, balls = bs, drawnWalls = ws, drawing = dwg } =
    pictures [ pictures $ map (drawBall white 2) bs
             , pictures $ map (drawWall white) $ case dwg of
                                                   Drawing dw -> dw:ws
                                                   NotDrawing -> ws
             , drawLevel l ]

drawLevel :: Level -> Picture
drawLevel Level { walls = ws, goal = g } =
    pictures $ map (drawWall (greyN 0.5)) ws -- drawGoal g :

drawGoal :: Goal -> Picture
drawGoal (p1@(p1x, p1y), p2@(p2x, p2y)) = color blue
                                          $ polygon [p1, (p1x, p2y), p2, (p2x, p1y)]

drawFutureBalls :: Float -> (Int, PlayState) -> Picture
drawFutureBalls maxN (n, PlayState { balls = bs }) = 
    let com = 1 - logBase maxN (fromIntegral n)
        clr = makeColor com com com com in
    pictures $ map (drawBall clr com) bs

drawFutures :: PlayState -> Picture
drawFutures = pictures . take 100 . map (drawFutureBalls 50000) .
              filter ((== 0) . (`mod` 15) . fst) . zip [1..] . iterate (stepPlay 0)

drawBall :: Color -> Float -> Ball -> Picture
drawBall clr th Ball { pos = (x, y), theta = t, radius = r, vel = (vx, vy) } =
    let (d1x, d1y) = rotateV t (2*r, 0)
        (d2x, d2y) = rotateV t (0, 2*r) in
    pictures [ color clr $ pictures [ translate x y $ thickCircle r th
                                    , line [(x - d1x/2, y - d1y/2), (x + d1x/2, y + d1y/2)]
                                    , line [(x - d2x/2, y - d2y/2), (x + d2x/2, y + d2y/2)] ]
             , color green $ line [(x, y), (x + vx*5, y + vy*5)] ]

drawWall :: Color -> Wall -> Picture
drawWall clr (Wall start end) = color clr $ line [start, end]
