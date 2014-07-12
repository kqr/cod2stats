{-# LANGUAGE OverloadedStrings #-}

module Models where

import           Database.PostgreSQL.Simple
import           Data.Text.Lazy                         (Text)
import           Data.Text.Format                       (format)
import qualified Data.Text.Format           as F

import           Control.Applicative                    (liftA2)
import           Data.Monoid                            (mconcat, (<>))
import           Data.Either                            (rights)
import           Data.Traversable                       (for)

import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Except      hiding (except)


--  ==============================
--  ==         Types            ==
--  ==============================

newtype Playtime = Playtime { fromPlaytime :: Integer }
toPlaytime :: Double -> Playtime
toPlaytime = Playtime . floor

data Player = Player { player_id  :: Int
                     , name       :: Text
                     , efficacy   :: Double
                     , killCount  :: Integer
                     , deathCount :: Integer
                     , kdr        :: Double
                     , playtime   :: Playtime
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
    [ ] -> Left empty
    [x] -> Right x
    _   -> Left longer




--  ==============================
--  ==        Getters           ==
--  ==============================

-- This type represents a monad where database queries can
-- be run and exceptions can be thrown
type PGTransaction a = ExceptT Text IO a


efficacyQuery :: Query
efficacyQuery = "roundplayers.kills*roundplayers.kills*60/EXTRACT('epoch' FROM roundplayers.playtime) AS efficacy "

getRoundEfficacy :: Connection -> Int -> Int -> PGTransaction Double
getRoundEfficacy pg player_id round_id = do
  row <- liftIO $ flip (query pg) (round_id, player_id) $
           "SELECT " <> efficacyQuery <> "FROM roundplayers WHERE round_id=? AND player_id=?"
  
  fmap fromOnly . except $ single row
                    (format "Player {} was not in round {}" (player_id, round_id))
                    (format "Too many hits on player {} in round {}. Check database integrity!"
                        (player_id, round_id))


getEfficacy :: Connection -> Int -> IO Double
getEfficacy pg player_id = do
  row <- flip (query pg) (Only player_id) $
           "SELECT AVG(efficacy) FROM (SELECT " <> efficacyQuery <>
           "FROM roundplayers WHERE player_id=? ORDER BY round_id DESC LIMIT 10) a"
  return . head . (++[0]) . map fromOnly $ row


getPlayer :: Connection -> Int -> PGTransaction Player
getPlayer pg player_id = do
  row <- liftIO $ query pg "SELECT name, EXTRACT('epoch' FROM playtime), kills, deaths FROM players WHERE id=?" (Only player_id)

  (player_name, playtime, kills, deaths) <-
          except $ single row
            (format "No player with id {} exists!" (F.Only player_id))
            (format "Too many hits on player id {}. Check database integrity!" (F.Only player_id))

  eff <- liftIO $ getEfficacy pg player_id

  return $ Player player_id player_name eff kills deaths (fromInteger kills/fromInteger deaths) (toPlaytime playtime)


getPlayerByName :: Connection -> Text -> PGTransaction Player
getPlayerByName pg player_name = do
  row <- liftIO $ query pg "SELECT id FROM players WHERE name=?" (Only player_name)

  Only player_id <-
          except $ single row
            (format "No player with name \"{}\" exists" (F.Only player_name))
            (format "Too many hits on player name \"{}\". Check database integrity!" (F.Only player_name))

  getPlayer pg player_id


getTopPlayers :: Connection -> IO [Player]
getTopPlayers pg = do
  players <- query_ pg $ mconcat
               [ "SELECT players.id "
               , "FROM   rounds, players, roundplayers "
               , "WHERE  roundplayers.id = rounds.id AND "
               , "       roundplayers.player_id = players.id "
               , "ORDER BY ( "
               , "  SELECT AVG(efficacy) "
               , "  FROM ( "
               , "    SELECT "
               ,               efficacyQuery
               , "    FROM     roundplayers "
               , "    WHERE    player_id=players.id "
               , "    ORDER BY round_id DESC "
               , "    LIMIT    10 " 
               , "  ) a "
               , ") DESC"
               ]

  fmap rights . for players $ runExceptT . getPlayer pg . fromOnly


getRound :: Connection -> Int -> PGTransaction Round
getRound pg round_id = do
  row          <- liftIO $ query pg "SELECT map FROM rounds WHERE id = ?" (Only round_id)

  Only mapName <- except $ single row
                    (format "There's no round with index {}!" (F.Only round_id))
                    (format "Too many hits on round index {}. Check database integrity." (F.Only round_id))

  playerInfo   <- liftIO . flip (query pg) (Only round_id) $ mconcat
                    [ "SELECT "
                    , "  players.id, players.name, EXTRACT('epoch' FROM roundplayers.playtime), "
                    , "  roundplayers.kills, roundplayers.deaths, "
                    ,    efficacyQuery
                    , "FROM "
                    , "  rounds, roundplayers, players "
                    , "WHERE "
                    , "  rounds.id = ? AND "
                    , "  roundplayers.round_id = rounds.id AND "
                    , "  roundplayers.player_id = players.id "
                    , "ORDER BY "
                    , "  efficacy DESC"
                    ]

  let players  = flip map playerInfo $ \(player_id, name, playtime, kills, deaths, efficacy) ->
                   Player player_id name efficacy kills deaths (fromInteger kills / fromInteger deaths) (toPlaytime playtime)

  return $ Round round_id mapName players


getLatestRounds :: Connection -> IO [Round]
getLatestRounds pg = do
  rounds <- query_ pg "SELECT id FROM rounds ORDER BY id DESC LIMIT 10"
  fmap rights . for rounds $ runExceptT . getRound pg . fromOnly

