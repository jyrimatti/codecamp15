{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Yesod

import GHC.Generics
import Data.Aeson
import Network.HTTP
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Binary.UTF8.String as U
import Data.List (minimumBy)
import Data.Graph.AStar
import Data.Set (fromList)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App

main :: IO ()
main = warp 3000 App

-- fixing IP addresses to the source, like a boss
server = "http://192.168.2.85:8080/"
myName = "Jyri-Matti"
client = "http://192.168.2.75:3000/"

register = do
     let d = U.decode $ BS.unpack $ encode $ Register myName client
     putStrLn d
     req <- simpleHTTP (postRequestWithBody (server ++ "register") "application/json" d)
     resp <- getResponseBody req
     putStrLn resp
     return ()

data Register = Register {
    playerName :: String,
    _url :: String 
} deriving (Show,Generic)
instance ToJSON Register where
   toJSON (Register playerName _url) = object ["playerName" .= playerName, "url" .= _url]
instance FromJSON Register

data RegisterResponse = RegisterResponse {
        _id :: String,
        player :: Player,
        _gameState :: GameState
} deriving (Show,Generic)
instance ToJSON RegisterResponse
instance FromJSON RegisterResponse where
    parseJSON (Object v) = RegisterResponse <$>
                            v .: "id" <*>
                            v .: "player" <*>
                            v .: "gameState"

data GameStateChanged = GameStateChanged {
    gameState :: GameState,
    playerState :: Player
} deriving (Show,Generic)
instance ToJSON GameStateChanged
instance FromJSON GameStateChanged

data GameMap = GameMap {
  width :: Int,
  height :: Int,
  tiles :: [String]
} deriving (Show,Generic)
instance ToJSON GameMap
instance FromJSON GameMap
    
data GameState  = GameState{
    map :: GameMap,
    players :: [Player],
    items :: [Item]
} deriving (Show,Generic)
instance ToJSON GameState
instance FromJSON GameState

data Player = Player {
    position :: Position,
    name :: String,
    url :: String,
    score :: Int,
    money :: Int,
    health :: Int,
    usableItems :: [Item]
} deriving (Show,Generic)
instance ToJSON Player
instance FromJSON Player

data Move =
        UP |
        DOWN |
        RIGHT |
        LEFT |
        PICK |
        USE deriving (Show,Generic)
instance ToJSON Move
instance FromJSON Move

data ItemType = JUST_SOME_JUNK | WEAPON deriving (Show,Generic)
instance ToJSON ItemType
instance FromJSON ItemType

data Item = Item {
  price :: Int,
  discountPercent :: Int,
  _position :: Position,
  _type :: ItemType,
  isUsable :: Bool
} deriving (Show,Generic)
instance ToJSON Item
instance FromJSON Item where
  parseJSON (Object v) = Item <$>
                            v .: "price" <*>
                            v .: "discountPercent" <*>
                            v .: "position" <*>
                            v .: "type" <*>
                            v .: "isUsable"

data Position = Position {
  x :: Int,
  y :: Int
} deriving (Show,Generic,Eq,Ord)
instance ToJSON Position
instance FromJSON Position

getHomeR :: Handler Value
getHomeR = do
    liftIO register
    returnJson ()

postHomeR :: Handler Value
postHomeR = do
    body <- parseJsonBody
    case body of
        Error s -> undefined
        Success r@(GameStateChanged gs ps) -> vastaa r
        
vastaa r = do
  --liftIO $ putStrLn $ U.decode $ BS.unpack $ encode r
  let vast = vastaus r
  liftIO $ putStrLn $ "********: " ++ show vast
  return $ toJSON vast
  
vastaus :: GameStateChanged -> Move
vastaus (GameStateChanged (GameState (GameMap width height tiles) players items) (Player position@(Position x y) name url score money health usableItems)) =
   if (pelaajanRuudussaItem money position items) then PICK else
       let pos = if (not $ any (ostettava money) items) then maali tiles else _position $ valitseItem money position items
           -- old stuff (without A*-search) commented away
           --suunnat = (paatteleToivottuSuunta position pos) ++ foo position pos ++ kaikkiViereiset position
           Just suunta = laske tiles pos position
           suunnat = [head suunta]
           vapaat = filter (vapaa tiles) suunnat
       in 
           suunnaksi position $ head vapaat

ostettava :: Int -> Item -> Bool
ostettava money (Item price discountPercent _ _ _) = todellinenHinta price discountPercent < money

pelaajanRuudussaItem money position items = any (\(price,discountPercent,pos) -> todellinenHinta price discountPercent < money && pos == position) (fmap (\(Item price discountPercent pos _ _) -> (price,discountPercent,pos)) items)

todellinenHinta :: Int -> Int -> Int
todellinenHinta price discountPercent = ceiling $ toRational price * (toRational (100 - discountPercent) / 100)

suunnaksi player@(Position px py) target@(Position sx sy)
  | px < sx = RIGHT
  | px > sx = LEFT
  | py < sy = DOWN
  | py > sy = UP 

vapaa tiles (Position x y) = tiles !! y !! x /= 'x'

maali :: [String] -> Position
maali tiles = head [Position x y | y <- [0..(length tiles - 1)], x <- [0..(length (tiles !! y) - 1)], tiles !! y !! x == 'o']

paatteleToivottuSuunta (Position px py) (Position ix iy) =
  if abs (px - ix) < abs (py - iy) then
      [Position px (py + 1) | py < iy] ++
      [Position px (py - 1) | py > iy] ++
      [Position (px-1) py] ++
      [Position (px+1) py]
  else 
      [Position (px+1) py | px < ix] ++
      [Position (px-1) py | px > ix] ++
      [Position px (py - 1)] ++
      [Position px (py + 1)]

kaikkiViereiset (Position px py) =
  [Position px (py - 1),
   Position px (py + 1),
   Position (px+1) py,
   Position (px-1) py] 

foo (Position px py) target@(Position sx sy) =
  if abs (px - sx) > abs (py - sy) then [Position px (py-1)] else [Position (px-1) py] 

valitseItem money (Position px py) items = minimumBy (\i1 i2 -> if arvo i1 > arvo i2 then LT else GT) (filter (\(Item price discountPercent _ _ _) -> todellinenHinta price discountPercent < money) items) 
    where arvo (Item price discountPercent (Position ix iy) _ _) = ceiling $ toRational (todellinenHinta price discountPercent) / toRational (abs (px-ix) + abs (py-iy))

laske tiles goal start = aStar (graph tiles) (\a b -> 1) (heur goal) (== goal) start 

heur goal@(Position gx gy) (Position x y) = abs (x-x) + abs (gy-gy)

graph tiles p@(Position x y) = fromList $ filter (vapaa tiles) (kaikkiViereiset p)