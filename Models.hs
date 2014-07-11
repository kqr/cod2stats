{-# LANGUAGE OverloadedStrings #-}

module Models where

import    Database.PostgreSQL.Simple
import    Data.Text.Lazy                    (Text)
import    Data.Time                         (DiffTime)

import    Data.Monoid                       (mconcat)
import    Data.Either                       (rights)
import    Data.Traversable                  (for)

import    Control.Monad.Trans.Class
import    Control.Monad.Trans.Except hiding (except)


--  ==============================
--  ==         Types            ==
--  ==============================

data Player = Player { player_id  :: Int
                     , name       :: Text
                     , efficacy   :: Double
                     , killCount  :: Integer
                     , deathCount :: Integer
                     , kdr        :: Double
                     , playtime   :: DiffTime
                     }


data Round  = Round  { round_id   :: Int
                     , mapName    :: Text
                     , players    :: [Player]
                     }



--  ==============================
--  ==       Utilities          ==
--  ==============================

except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

-- Extract a single result from a list of rows,
-- throwing errors if there's more or less than
-- a single result!
single :: [r] -> e -> e -> Either e r
single xs empty longer =
  case xs of
    []  -> Left empty
    [x] -> Right x
    _   -> Left longer




--  ==============================
--  ==        Getters           ==
--  ==============================

-- This type represents a monad where database queries can
-- be run and exceptions can be thrown
type PGTransaction a = ExceptT Text IO a


getPlayer :: Connection -> Int -> PGTransaction Player
getPlayer pg player_id = do
  rows <- lift $ query pg "SELECT name, playtime, kills, deaths FROM players WHERE id=?" (Only player_id)

  (player_name, playtime, kills, deaths) <-
          except $ single rows
            "No player with that ID exists!"
            "Too many hits. Check database integrity!"

  return $ Player player_id player_name (-1) kills deaths (fromInteger kills/fromInteger deaths) playtime


getPlayerByName :: Connection -> Text -> PGTransaction Player
getPlayerByName pg player_name = do
  row <- lift $ query pg "SELECT id FROM players WHERE name=?" (Only player_name)

  Only player_id <-
          except $ single row
            "No player with that name exists"
            "Too many hits. Check database integrity!"

  getPlayer pg player_id


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
  row            <- lift $ query pg "SELECT map FROM rounds WHERE id = ?" (Only round_id)

  (Only mapName) <- except $ single row
                      "There's no row with that index!"
                      "Too many hits. Check database integrity."

  playerInfo     <- lift . flip (query pg) (Only round_id) $ mconcat
                      [ "SELECT "
                      , "  players.id, players.name, "
                      , "  roundplayers.efficacy, roundplayers.playtime, "
                      , "  roundplayers.kills, roundplayers.deaths "
                      , "FROM "
                      , "  rounds, roundplayers, players "
                      , "WHERE "
                      , "  rounds.id = ? AND "
                      , "  roundplayers.round_id = rounds.id AND "
                      , "  roundplayers.player_id = players.id "
                      , "ORDER BY "
                      , "  roundplayers.efficacy DESC"
                      ]

  let players = flip map playerInfo $ \(player_id, name, efficacy, playtime, kills, deaths) ->
                  Player player_id name efficacy kills deaths (fromInteger kills / fromInteger deaths) playtime 

  return $ Round round_id mapName players

