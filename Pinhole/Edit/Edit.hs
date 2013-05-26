{-# LANGUAGE GADTs, GADTSyntax #-}

module Edit.Edit
( EditState (EditState)
, editingPlay
, currentTool
, prevStates

, Tool (Make, Making)

, drawEdit
, handleEditEvent
, stepEdit ) where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.Pure.Game

import Level.Level
import Play.PlayState
import Play.Draw

data EditState = EditState {
      editingPlay :: PlayState

    , currentTool :: Tool

    , prevStates :: [EditState] }

data Tool where
    Make :: Element a => (Point -> Point -> a) -> Tool
    Making :: Element a => Tool -> (Point -> a) -> a -> Tool

class Element a where
    add :: a -> Level -> Level
    unadd :: a -> Level -> Level

instance Element Wall where
    add w l@(Level { walls = ws }) = l { walls = w:ws }
    unadd w l@(Level { walls = w':ws })
        | w' == w   = l { walls = ws }
        | otherwise = error "unadding wrong"

drawEdit :: EditState -> Picture
drawEdit (EditState { editingPlay = pl }) = Pictures [ Color white $ Text "Edit"
                                                     , drawPlay pl ]

handleEditEvent :: Event -> EditState -> EditState
handleEditEvent (EventKey (MouseButton LeftButton) ks _ mPos) = handleLeftMouse ks mPos
handleEditEvent (EventMotion mPos) = handleMotion mPos
handleEditEvent _ = id

handleLeftMouse :: KeyState -> Point -> EditState -> EditState
handleLeftMouse Down mPos ed@(EditState {
                                editingPlay = pl@(PlayState { level = l })
                              , currentTool = tool@(Make elCtr) }) =
    let elTo = elCtr mPos
        el = elTo mPos in
    ed { editingPlay = pl { level = add el l }
       , currentTool = Making tool elTo el }

handleLeftMouse Up _ ed@(EditState { currentTool = Making tool _ _ }) =
    ed { currentTool = tool }

handleMotion :: Point -> EditState -> EditState
handleMotion mPos ed@(EditState { editingPlay = pl@(PlayState { level = l })
                                , currentTool = Making tool elTo el }) =
    let el' = elTo mPos in
    ed { editingPlay = pl { level = add el' $ unadd el l }
       , currentTool = Making tool elTo el' }
handleMotion _ ed = ed

stepEdit :: Float -> EditState -> EditState
stepEdit _ = id
