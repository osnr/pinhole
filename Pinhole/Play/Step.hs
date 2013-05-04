module Play.Step ( stepPlay, stepBall ) where

import Play.World
import Play.Collisions

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
