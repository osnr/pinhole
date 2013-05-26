module GameState
( GameState (Intro, IntroLevel, PlayLevel, EditLevel)

, draw
, handleEvent
, step

, playLevel
, edit ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Level.Level
import Play.Step
import Play.Draw
import Play.PlayState
import Play.Events

import Edit.Edit

type Countdown = Float

data GameState = Intro Countdown |
                 IntroLevel Countdown Level |
                 PlayLevel PlayState |
                 EditLevel EditState

draw :: GameState -> Picture
draw (Intro _) = Translate (-160) 0 . Color white . Scale 0.5 0.5 $ Text "Pinhole"
draw (IntroLevel _ _) = Text "Level 1"
draw (PlayLevel pl) = drawPlay pl
draw (EditLevel ed) = drawEdit ed

handleEvent :: Event -> GameState -> GameState
handleEvent _ (Intro t) = Intro t
handleEvent _ (IntroLevel t l) = IntroLevel t l
handleEvent (EventKey (Char 'e') Up _ _) pll@(PlayLevel _) = edit pll
handleEvent (EventKey (Char 'e') Up _ _) edl@(EditLevel _) = unedit edl
handleEvent (EventKey (Char 'r') Up _ _) (PlayLevel (PlayState { level = l })) = playLevel l
handleEvent e (PlayLevel pl) = PlayLevel $ handlePlayEvent e pl
handleEvent e (EditLevel ed) = EditLevel $ handleEditEvent e ed

step :: Float -> GameState -> GameState
step dt (Intro t) | t > 5 = playLevel initialLevel
                  | otherwise = Intro (t + dt)
step dt (IntroLevel t l) = IntroLevel (t + dt) l
step dt (PlayLevel pl) = PlayLevel $ stepPlay dt pl
step dt (EditLevel ed) = EditLevel $ stepEdit dt ed

playLevel :: Level -> GameState
playLevel l = PlayLevel PlayState {
                level = l

              , balls = [ Ball { pos = spawn l
                               , vel = (0, 0)
                               , theta = 0
                               , omega = 0
                               , radius = 20 } ]
              , drawnWalls = []
              , drawing = NotDrawing
              , future = False }

edit :: GameState -> GameState
edit (PlayLevel pl) = 
    EditLevel $ EditState { editingPlay = pl
                          , currentTool = Make Wall
                          , prevStates = [] }

unedit :: GameState -> GameState
unedit (EditLevel (EditState { editingPlay = pl })) = PlayLevel pl
