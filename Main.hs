{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty
import           Database.PostgreSQL.Simple            (connectPostgreSQL)
import qualified Network.HTTP.Types            as HTTP (status404)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Except            (runExceptT)

import           Data.Monoid                           ((<>))
import           Data.Text.Lazy                        (Text)
import           Data.Text.Encoding               (encodeUtf8)
import           Text.Blaze.Html.Renderer.Text         (renderHtml)

import           System.Environment                    (getEnv)
import           Web.Heroku                            (dbConnParams)

import           Models
import           Views


main = do
  port <- fmap read (getEnv "PORT")

  params <- dbConnParams
  let connStr = foldr (\(k, v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
  postgres <- connectPostgreSQL connStr

  scotty port $ do

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




