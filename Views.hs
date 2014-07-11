{-# LANGUAGE OverloadedStrings #-}

module Views where

import Web.Scotty
import Control.Monad.IO.Class     (liftIO)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Format

import Models


playerView :: Player -> ActionM ()
playerView player =
  text $ T.unlines [ format "Player {}" (Only (name player))
                   , format "Kills: {}" (Only (killCount player))
                   , format "Deaths: {}" (Only (deathCount player))
                   , format "KDR: {}" (Only (fixed 2 (kdr player)))
                   , format "Time played: {}" (Only (playtime player))
                   ]

playerListView :: [Player] -> ActionM ()
playerListView players =
  text . T.unlines . flip map players $ \p -> format "{} ({} kills, {} deaths)" (name p, killCount p, deathCount p)


roundView :: Round -> ActionM ()
roundView round =
  text . T.unlines $ [ format "Round played on {}" (Only (mapName round))
                     , ""
                     , "Players: "
                     ] ++ scoreboard 
  where
    scoreboard = map playerscore (players round) 
    playerscore p = format "{}\t\t(Efficacy {}\t{} kills\t{} deaths\t{} K/D\t{} played)"
                       (name p, efficacy p, killCount p, deathCount p, kdr p, playtime p)


