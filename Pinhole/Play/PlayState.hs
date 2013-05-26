module Play.PlayState
( PlayState (PlayState)
, level
, balls
, drawnWalls
, drawing
, future

, Ball (Ball)
, pos
, vel
, theta
, omega
, radius

, DrawState (Drawing, NotDrawing) ) where

import Graphics.Gloss.Data.Vector

import Level.Level

data PlayState = PlayState {
      level :: Level

    , balls :: [Ball]
    , drawnWalls :: [Wall]

    , drawing :: DrawState
    , future :: Bool
    } deriving (Show)

data Ball = Ball {
      pos :: Vector
    , vel :: Vector
    , theta :: Float
    , omega :: Float
    , radius :: Float
    } deriving (Show)

data DrawState = Drawing Wall | NotDrawing
                 deriving (Show)
