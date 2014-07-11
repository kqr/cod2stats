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
                   , "Win–Loss: TBI"
                   , format "Kills: {}" (Only (killCount player))
                   , format "Deaths: {}" (Only (deathCount player))
                   , format "KDR: {}" (Only (fixed 2 (kdr player)))
                   , format "Headshots: {}" (Only (hsCount player))
                   , format "Time played: {}" (Only (playtime player))
                   ]

playerListView :: [Player] -> ActionM ()
playerListView players =
  text . T.unlines . flip map players $ \p -> format "{} ({} kills, {} deaths)" (name p, killCount p, deathCount p)




