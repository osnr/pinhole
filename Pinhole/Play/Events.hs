module Play.Events ( handlePlayEvent ) where

import Graphics.Gloss.Interface.Pure.Game

import Level.Level
import Play.PlayState

handlePlayEvent :: Event -> PlayState -> PlayState
handlePlayEvent event pl@(PlayState { drawing = dwg
                                    , drawnWalls = ws
                                    , future = ft
                                    , paused = pd }) =
    case event of
      EventKey (MouseButton LeftButton) Down _ mPos ->
          case dwg of
            NotDrawing -> pl { drawing = Drawing $ Wall mPos mPos }
            Drawing _ -> pl
      EventKey (MouseButton LeftButton) Up _ _ ->
          case dwg of
            NotDrawing -> pl
            Drawing w@(Wall start end) ->
                pl {
                     drawnWalls = if (end - start) /= 0
                             then w:ws
                             else ws
                   , drawing = NotDrawing }
      EventMotion mPos ->
          case dwg of
            NotDrawing -> pl
            Drawing (Wall start _) -> pl { drawing = Drawing $ Wall start mPos }
      EventKey (Char 'f') Up _ _ -> -- see/unsee the future
          pl { future = not ft }
      EventKey (Char 'p') Up _ _ -> -- pause
          pl { paused = not pd }
      EventKey (Char 'r') Up _ _ -> -- restart
          pl { balls = [initBall (level pl)], drawnWalls = [] }
      _ -> pl
