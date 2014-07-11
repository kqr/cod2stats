{-# LANGUAGE OverloadedStrings #-}

module Views where

import           Prelude                     hiding (head, div, id)

import           Text.Blaze.Html5            hiding (map)
import           Text.Blaze.Html5.Attributes hiding (name, title)
import           Data.String                        (fromString)
import           Data.Text.Lazy                     (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Format
import           Data.Text.Format.Params            (Params)

import           Control.Monad                      (forM_)
import           Data.Monoid                        ((<>))
import           Data.Time                          (DiffTime)

import Models


-- Text.Format.format but producing Blaze HTML instead
formatHtml :: Params ps => Format -> ps -> Html
formatHtml fmt = toHtml . format fmt

formatAttr :: Params ps => Format -> ps -> AttributeValue
formatAttr fmt = fromString . T.unpack . format fmt


-- A useful class for quickly converting data to HTML
class    Displayable a        where display :: a -> Html

instance Displayable Text     where display = toHtml
instance Displayable Integer  where display = formatHtml "{}" . Only
instance Displayable Int      where display = formatHtml "{}" . Only
instance Displayable Double   where display = formatHtml "{}" . Only . fixed 2
instance Displayable DiffTime where
  display dt | h > 0     = formatHtml "{} hours" (Only (fixed 1 hf))
             | otherwise = formatHtml "{}:{}" (m, left 2 '0' s)
    where s = floor dt               `mod` 60 :: Int
          m = (floor dt `quot` 60)   `mod` 60 :: Int
          h = (floor dt `quot` 3600)          :: Int
          hf = fromRational (toRational dt) / 3600 :: Double





scoreboard :: [Player] -> Html
scoreboard players = do
  tr $ mapM_ th ["Player", "Efficacy", "Kills", "Deaths", "KDR", "Time played"]
  forM_ players $ \player -> tr $ do
    let playerUrl = formatAttr "/player/{}" (Only (name player))
    td $ a ! href playerUrl $ display (name player)
    td $ display (efficacy player)
    td $ display (killCount player)
    td $ display (deathCount player)
    td $ display (kdr player)
    td $ display (playtime player)




playerView :: Player -> Html
playerView player = do
  h2 $ formatHtml "Player {}" (Only (name player))
  table $ do
    tr $ th "Efficacy: "    >> td (display (efficacy player))
    tr $ th "Kills: "       >> td (display (killCount player))
    tr $ th "Deaths: "      >> td (display (deathCount player))
    tr $ th "KDR: "         >> td (display (kdr player))
    tr $ th "Time played: " >> td (display (playtime player))

playerListView :: [Player] -> Html
playerListView players = do
  h2 "Top 10 players"
  table $ scoreboard players



roundView :: Round -> Html
roundView round = do
  div ! class_ "table" $ do
    h2 $ formatHtml "Round {} on {} ({} players)" (round_id round, mapName round, length (players round))
    table $ scoreboard (players round)


roundListView :: [Round] -> Html
roundListView rounds = do
  div ! class_ "table" $ do
    h2 "Last 10 rounds played"
    table $ do
      tr $ mapM_ th ["Round #", "Map", "Players"]

      forM_ rounds $ \round -> tr $ do
        let roundUrl = formatAttr "/round/{}" (Only (round_id round))
        td $ a ! href roundUrl $ display (round_id round)
        td $ display (mapName round)
        td $ display (length (players round))




errorView :: Text -> Html
errorView err = docTypeHtml $ do
  p $ toHtml (format "Error: {}" (Only err))



site :: Html -> Html
site contents =
  docTypeHtml $ do
    head $ do
      title "cod2stats"
      link ! rel "stylesheet" ! href "/css"
    body $ do
      header $ do
        h1 $ a ! href "/" $ "cod2stats"
        nav $ do
          ul $ do
            li $ a ! href "/players" $ "Players"
            li $ a ! href "/rounds" $ "Rounds"

      div ! id "main" $ contents

      footer $ p $ do
          "2014 © kqr, "
          a ! href "https://github.com/kqr/cod2stats" ! target "_BLANK" $ "source"

