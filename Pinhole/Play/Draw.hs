module Play.Draw
( draw
, drawPlay
, drawPlayState
, drawBall
, drawWall ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Level.Level

import Play.PlayState
import Play.Step

doc :: Picture
doc = color white $ pictures [ text "draw walls by dragging"
                             , translate 0 (-160) $ text "press F to see the future"
                             , translate 408 (-160 * 2) $ text "P to pause"
                             , translate 408 (-160 * 3) $ text "R to restart" ]

draw :: PlayState -> Picture
draw pl = case docState pl of
            DocShownFor _ -> withDoc
            DocDone       -> if paused pl
                             then withDoc
                             else drawPlay pl
    where withDoc = pictures [ translate 100 (-150) $ scale 0.1 0.1 $ doc
                             , drawPlay pl ]

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
drawLevel Level { walls = ws } =
    pictures $ map (drawWall (greyN 0.5)) ws -- drawGoal g :

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
