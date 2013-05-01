module World
( World (World)
, Ball (Ball)
, balls
, walls
, drawing
, Wall (Wall)
, pos
, vel
, radius
, DrawState (Drawing, NotDrawing) ) where

data World = World {
      balls :: [Ball]
    , walls :: [Wall]
    , drawing :: DrawState
    } deriving (Show)

data Ball = Ball {
      pos :: (Float, Float)
    , vel :: (Float, Float)
    , radius :: Float
    } deriving (Show)

data Wall = Wall (Float, Float) (Float, Float)
            deriving (Show)

data DrawState = Drawing Wall | NotDrawing
                 deriving (Show)
