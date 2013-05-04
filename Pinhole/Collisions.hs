module Collisions ( collideWB ) where
import Debug.Trace
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line

import World

closestPointOnSeg :: Point -> Point -> Point -> Point
closestPointOnSeg sa sb c
    | proj <= 0 = sa
    | proj >= magV sv = sb
    | otherwise = (proj `mulSV` svu) + sa
    where sv = sb `subV` sa
          ptv = c `subV` sa

          svu = normaliseV sv
          proj = ptv `dotV` svu

offsetSegCircle :: Point -> Vector -> Float -> Vector
offsetSegCircle pt c r
    | magV dist_v > r = (0, 0)
    | magV dist_v <= 0 = (0, 0)
    | otherwise = ((r - magV dist_v) / magV dist_v) `mulSV` dist_v
          where dist_v = c `subV` pt

addV :: Vector -> Vector -> Vector
addV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subV :: Vector -> Vector -> Vector
subV (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

normal :: Float
normal = 1.10

reflectWB :: Wall -> Point -> Ball -> Ball
reflectWB (Wall start end) pt b@(Ball { pos = c, vel = v, theta = t, radius = r }) =
    let n = mulSV normal . normaliseV $ c `subV` pt
        v' = v `subV` ((v `dotV` n) `mulSV` n)
        l_sig = v `detV` n in
    b { vel = v'
      , theta = t - 2 * l_sig / r }

collideWB :: Wall -> Ball -> Ball
collideWB w@(Wall start end) b@(Ball { pos = c, radius = r })
    | (end `subV` start) == (0, 0) = b
    | otherwise = let pt = closestPointOnSeg start end c
                      o = offsetSegCircle pt c r in
                  if o /= (0, 0)
                  then let b' = b { pos = c `addV` o } in
                       reflectWB w pt b'
                  else b

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
