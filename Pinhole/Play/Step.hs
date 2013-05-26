module Play.Step ( stepPlay, stepBall ) where

import Level.Level
import Play.PlayState

import Play.Collisions

stepPlay :: Float -> PlayState -> PlayState
stepPlay dt pl@(PlayState { level = l, balls = bs, drawnWalls = ws, drawing = dwg }) =
    pl { balls = let ws' = walls l ++ case dwg of
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
