module Play.Step ( step, stepPlay, stepBall ) where

import Level.Level
import Play.PlayState

import Play.Collisions

step :: Float -> PlayState -> PlayState
step dt pl = if paused pl
             then pl
             else stepPlay dt pl

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
