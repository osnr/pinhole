module Collisions ( collideWB ) where
import Debug.Trace
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line

import World

closestPointOnSeg :: Point -> Point -> Point -> Point
closestPointOnSeg sa@(ax, ay) sb@(bx, by) (cx, cy)
    | proj <= 0 = sa
    | proj >= magV sv = sb
    | otherwise = (proj `mulSV` svu) + sa
    where sv = (bx - ax, by - ay)
          ptv = (cx - ax, cy - ay)

          svu = normaliseV sv
          proj = ptv `dotV` svu

offsetSegCircle :: Vector -> Vector -> Vector -> Float -> Vector
offsetSegCircle sa sb c@(cx, cy) r
    | magV dist_v > r = (0, 0)
    | magV dist_v <= 0 = (0, 0)
    | otherwise = ((r - magV dist_v) / magV dist_v) `mulSV` dist_v
          where (clx, cly) = closestPointOnSeg sa sb c
                dist_v = (cx - clx, cy - cly)

reflectWB :: Wall -> Ball -> Ball
reflectWB (Wall (sx, sy) (ex, ey)) b =
b { vel = let ballAngle = argV v
                                                     wallAngle = argV (ex - sx, ey - sy)
                                                     

collideWB :: Wall -> Ball -> Ball
collideWB w@(Wall start end) b@(Ball { pos = center@(cx, cy), vel = v, radius = cr }) =
    let (ox, oy) = offsetSegCircle start end center cr
        b' = b { pos = (cx + ox, cy + oy) } in
    reflectWB w b'

{-collideWB (Wall (sx, sy) (ex, ey))
          ball@(Ball { pos = (bx, by), vel = v, radius = br }) =
    let x1 = sx - bx
        y1 = sy - by

        x2 = ex - bx
        y2 = ey - by

        dx = x2 - x1
        dy = y2 - y1
        dr = sqrt $ dx**2 + dy**2
        d = x1*y2 - x2*y1

        delta = (br * 0.9)**2 * dr**2 - d**2 in
    if delta >= 0
    then trace "hi"
         ball { vel = let ballAngle = argV v
                          wallAngle = argV (ex - sx, ey - sy)
                          ballAngle' = 2 * (wallAngle - ballAngle) + 180 in
                      magV v `mulSV` unitVectorAtAngle ballAngle' }
    else ball-}