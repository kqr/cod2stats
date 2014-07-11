{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty
import           Database.PostgreSQL.Simple            (connectPostgreSQL)
import qualified Network.HTTP.Types            as HTTP (status404)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Except            (runExceptT)

import           Data.Text.Lazy                        (Text)
import           Text.Blaze.Html.Renderer.Text         (renderHtml)

import           Models
import           Views


main = do
  postgres <- connectPostgreSQL "host='localhost' dbname='cod2stats' user='cod2stats' password='cod2stats'"

  scotty 3000 $ do

    get "/" $ do
      players <- liftIO $ getTopPlayers postgres
      showView (playerListView players)

    get "/rounds" $ do
      rounds <- liftIO $ getLatestRounds postgres
      showView (roundListView rounds)

    get "/round/:id" $ do
      round_id <- param "id"
      round    <- runPGTransaction (getRound postgres round_id)
      showView (either errorView roundView round)

    get "/players" $ do
      players <- liftIO $ getTopPlayers postgres
      showView (playerListView players)

    get "/player/id/:id" $ do
      player_id <- param "id"
      player    <- runPGTransaction (getPlayer postgres player_id)
      showView (either errorView playerView player)
      
    get "/player/:name" $ do
      playerName <- param "name"
      player     <- runPGTransaction (getPlayerByName postgres playerName)
      showView (either errorView playerView player)

    get "/css" $ file "main.css"

    notFound $ do
      status HTTP.status404
      text "404 lol"


  where
    runPGTransaction = liftIO . runExceptT

    showView contents = do
      setHeader "charset" "utf-8"
      html (renderHtml (site contents))




