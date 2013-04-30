module Collisions ( collideWB ) where


collide :: Wall -> Ball -> Ball
collide (Wall (sx, sy) (ex, ey))
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
    else ball
