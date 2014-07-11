{-# LANGUAGE OverloadedStrings #-}

module Models where

import Database.PostgreSQL.Simple
import Data.Text.Lazy (Text)
import Data.Time (DiffTime)

import Data.Monoid (mconcat)
import Data.Either (rights)
import Data.Traversable (for)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except



data Player = Player { player_id  :: Int
                     , name       :: Text
                     , efficacy   :: Double
                     , killCount  :: Integer
                     , deathCount :: Integer
                     , hsCount    :: Integer
                     , kdr        :: Double
                     , playtime   :: DiffTime
                     }


data Round  = Round  { round_id   :: Int
                     , mapName    :: Text
                     , players    :: [Player]
                     }



type PGTransaction a = ExceptT Text IO a


getPlayer :: Connection -> Int -> PGTransaction Player
getPlayer pg player_id = do
  res <- lift $ query pg "SELECT name, playtime FROM players WHERE id=?" (Only player_id)

  (player_name, playtime) <- case res of
                               [(player_name, playtime)] -> return (player_name, playtime)
                               _ -> throwE "No player with that ID exists!"

  [Only kills]  <- lift $ query pg "SELECT COUNT(*) FROM deaths WHERE killer_id=?" (Only player_id)
  [Only hs]     <- lift $ query pg "SELECT COUNT(*) FROM deaths WHERE (killer_id, headshot) = (?, true)" (Only player_id)
  [Only deaths] <- lift $ query pg "SELECT COUNT(*) FROM deaths WHERE dead_id=?" (Only player_id)

  return $ Player player_id player_name (-1) kills deaths hs (fromInteger kills/fromInteger deaths) playtime


getPlayerByName :: Connection -> Text -> PGTransaction Player
getPlayerByName pg player_name = do
  res <- lift $ query pg "SELECT id FROM players WHERE name=?" (Only player_name)
  case res of
    [(Only player_id)] -> getPlayer pg player_id
    _ -> throwE "No player with that name exists!"


getTopPlayers :: Connection -> IO [Player]
getTopPlayers pg = do
  players <- query_ pg $ mconcat
               [ "SELECT   players.id "
               , "FROM     players, deaths "
               , "WHERE    players.id = deaths.killer_id "
               , "GROUP BY players.id "
               , "ORDER BY COUNT(players.name) DESC "
               , "LIMIT    10"
               ]
  fmap rights . for players $ runExceptT . getPlayer pg . fromOnly


getRound :: Connection -> Int -> PGTransaction Round
getRound pg round_id = do
  res        <- lift $ query pg "SELECT map FROM rounds WHERE id = ?" (Only round_id)

  mapName    <- case res of
                  [(Only mapName)] -> return mapName
                  _                -> throwE "There's no round with that index!"

  playerInfo <- lift . flip (query pg) (Only round_id) $ mconcat
                  [ "SELECT "
                  , "  players.id, players.name, "
                  , "  roundplayers.efficacy, roundplayers.playtime "
                  , "FROM "
                  , "  rounds, roundplayers, players "
                  , "WHERE "
                  , "  rounds.id = ? AND "
                  , "  roundplayers.round_id = rounds.id AND "
                  , "  roundplayers.player_id = players.id"
                  ]

  let players = flip map playerInfo $ \(player_id, name, efficacy, playtime) ->
                  Player player_id name efficacy 0 0 0 0 playtime 

  return $ Round round_id mapName players

