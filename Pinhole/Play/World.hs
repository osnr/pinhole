module Play.World
( World (World)
, Ball (Ball)
, balls
, walls
, drawing
, future
, Wall (Wall)
, pos
, vel
, theta
, omega
, radius
, DrawState (Drawing, NotDrawing) ) where

data World = World {
      balls :: [Ball]
    , walls :: [Wall]
    , drawing :: DrawState
    , future :: Bool
    } deriving (Show)

data Ball = Ball {
      pos :: (Float, Float)
    , vel :: (Float, Float)
    , theta :: Float
    , omega :: Float
    , radius :: Float
    } deriving (Show)

data Wall = Wall (Float, Float) (Float, Float)
            deriving (Show)

data DrawState = Drawing Wall | NotDrawing
                 deriving (Show)
