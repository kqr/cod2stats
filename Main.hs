{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty
import           Database.PostgreSQL.Simple         (connectPostgreSQL)
import qualified Network.HTTP.Types         as HTTP (status404)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Except         (runExceptT)

import           Data.Text.Lazy                     (Text)

import           Models
import           Views



main = do
  postgres <- connectPostgreSQL "host='localhost' dbname='cod2stats' user='cod2stats' password='cod2stats'"

  scotty 3000 $ do
    get "/" $ do
      text "Hello, world!"

    get "/players" $ do
      players <- liftIO $ getTopPlayers postgres
      playerListView players

    get "/player/id/:id" $ do
      playerID <- param "id"
      player   <- runPQTransaction (getPlayer postgres playerID)
      either text playerView player
      
    get "/player/:name" $ do
      playerName <- param "name"
      player     <- runPQTransaction (getPlayerByName postgres playerName)
      either text playerView player

    notFound $ do
      status HTTP.status404
      html "404 lol"


  where
    runPQTransaction = liftIO . runExceptT



