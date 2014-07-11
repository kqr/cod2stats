{-# LANGUAGE OverloadedStrings #-}

module Views where

import Web.Scotty
import Control.Monad.IO.Class     (liftIO)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Format

import Models


playerSummary :: Player -> Text
playerSummary p =
  format "{}\t\t(Efficacy {}\t{} kills\t{} deaths\t{} K/D\t{} played)"
    (name p, fixed 2 (efficacy p), killCount p, deathCount p, fixed 2 (kdr p), playtime p) 

scoreboard :: [Player] -> [Text]
scoreboard = map playerSummary


playerView :: Player -> ActionM ()
playerView player =
  text $ T.unlines [ format "Player       {}" (Only (name player))
                   , format "Efficacy:    {}" (Only (fixed 2 (efficacy player)))
                   , format "Kills:       {}" (Only (killCount player))
                   , format "Deaths:      {}" (Only (deathCount player))
                   , format "KDR:         {}" (Only (fixed 2 (kdr player)))
                   , format "Time played: {}" (Only (playtime player))
                   ]

playerListView :: [Player] -> ActionM ()
playerListView =
  text . T.unlines . scoreboard


roundView :: Round -> ActionM ()
roundView round =
  text . T.unlines $ [ format "Round played on {}" (Only (mapName round))
                     , ""
                     , format "{} players: " (Only (length (players round)))
                     ] ++ scoreboard (players round) 

roundListView :: [Round] -> ActionM ()
roundListView rounds =
  text . T.unlines . flip map rounds $ \r ->
    format "Round {} on {} with {} players" (round_id r, mapName r, length (players r))

