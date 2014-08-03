{-# LANGUAGE DeriveGeneric #-}

module Level.Level
( Level (Level)
, spawn
, goal
, walls

, Goal
, Wall (Wall)

, initialLevel )
where

import Graphics.Gloss.Data.Point
import Data.Aeson
import GHC.Generics

screenWidth :: Num a => a
screenWidth = 640

screenHeight :: Num a => a
screenHeight = 480

data Level = Level {
      spawn :: Point
    , goal :: Goal

    , walls :: [Wall]
} deriving (Show, Generic)

instance FromJSON Level
instance ToJSON Level

type Goal = (Point, Point)

data Wall = Wall Point Point
            deriving (Show, Eq, Generic)

instance FromJSON Wall
instance ToJSON Wall

initialLevel :: Level
initialLevel =
    Level { spawn = (0, 240)
          , goal = ((0, -240), (20, 0))

          , walls = [ Wall (-screenWidth/2, -screenHeight/2 + 1)
                               (-screenWidth/3, -screenHeight/2 + 1) -- bottom-left
                    , Wall (-screenWidth/4, -screenHeight/2 + 1)
                               (screenWidth/2, -screenHeight/2 + 1) -- bottom-right
                    , Wall (-screenWidth/2 + 1, -screenHeight/2)
                               (-screenWidth/2 + 1, screenHeight/2) -- left
                    , Wall (screenWidth/2 - 1, -screenHeight/2)
                               (screenWidth/2 - 1, screenHeight/2) -- right
                    ] }
