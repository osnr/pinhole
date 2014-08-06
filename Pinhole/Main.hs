import System.Environment
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as B
import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty

import Graphics.Gloss.Interface.IO.Game

import Level.Level

import Play.PlayState
import Play.Events
import Play.Step
import Play.Draw

screenWidth :: Num a => a
screenWidth = 640

screenHeight :: Num a => a
screenHeight = 480

loadLevel :: FilePath -> IO Level
loadLevel path =
    let loadLevel' = do
          file <- B.readFile path
          let argLevelM = decode file
          case argLevelM of
            Just lvl -> return lvl
            Nothing  -> throwIO . userError $ "Invalid level file format"
    in do lvlE <- try loadLevel'
          case (lvlE :: Either SomeException Level) of
            Left _    -> return initialLevel -- we don't print to stderr/stdout because it will crash Windows app
            Right lvl -> return lvl

pinholeDirectory :: IO FilePath
pinholeDirectory = do
  docsDir <- getUserDocumentsDirectory
  let dir = docsDir </> "Pinhole"
  createDirectoryIfMissing False dir
  return dir

main :: IO ()
main = do args <- getArgs
          lvl <- case args of
                   [] -> do
                     dir <- pinholeDirectory
                     let path = dir </> "level.pinhole"
                     levelExists <- doesFileExist path
                     if levelExists
                       then loadLevel path
                       else return initialLevel
                   path : _ -> loadLevel path

          playIO (InWindow "Pinhole" (screenWidth, screenHeight) (0, 0))
                 black
                 60
                 (playLevel lvl)
                 (return . draw)
                 handleEvent
                 (\dt pl -> return $ step dt pl)

handleEvent :: Event -> PlayState -> IO PlayState
handleEvent event pl =
    do case event of
         EventKey (Char 's') Up _ _ -> save pl 0
         _ -> return ()
       return $ handlePlayEvent event pl

save :: PlayState -> Int -> IO ()
save pl i = do let lvl = level pl
                   lvl' = lvl { walls = drawnWalls pl ++ walls lvl }
                   file = encodePretty lvl'

                   suffix = show i
                   suffix' = if length suffix < 2 then '0':suffix else suffix
                   fileName = addExtension ("level_" ++ suffix') "pinhole"

               dir <- pinholeDirectory
               let path = dir </> fileName

               alreadyExists <- doesFileExist path
               if alreadyExists
                 then save pl (i + 1)
                 else B.writeFile path file
