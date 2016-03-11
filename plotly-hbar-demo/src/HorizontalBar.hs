{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HorizontalBar
  ( ItemMap
  , Plot (..)
  , emptyItemMap
  , addItemBin
  , renderItems
  , plottify
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Random (randomRIO)
import Text.Printf (printf)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Plotly.JSON (Type (..))

type BinLabel  = Text
type ItemLabel = Text

type ItemMap = Map ItemLabel (Color, BinMap)
type BinMap  = Map BinLabel Float

data Item = Item
    { x           :: ![Float]
    , y           :: ![BinLabel]
    , name        :: !ItemLabel
    , type_       :: !Type
    , orientation :: !Orientation
    , marker      :: !Marker
    }
    deriving Show

data Marker = Marker
    { color :: !Color
    , width :: !Int
    }
    deriving Show

data Orientation = Horizontal
    deriving Show

data Barmode = Stack
    deriving Show

data Color = RGB !Int !Int !Int
    deriving Show

data Plot = Plot { data_ :: ![Item]}
    deriving Show

emptyItemMap :: ItemMap
emptyItemMap = Map.empty

addItemBin :: ItemLabel -> BinLabel -> Float -> ItemMap -> IO ItemMap
addItemBin iLabel bLabel value iMap =
  case Map.lookup iLabel iMap of
    Just (c, m)  ->
      return $ Map.insert iLabel (c, (addBinValue bLabel value m)) iMap
    Nothing      -> do
      c <- randomColor
      return $ Map.insert iLabel
                     (c, (addBinValue bLabel value Map.empty)) iMap

addBinValue :: BinLabel -> Float -> BinMap -> BinMap
addBinValue label = Map.insertWith (+) label

renderItems :: ItemMap -> [Item]
renderItems = map renderItem . Map.toList

renderItem :: (ItemLabel, (Color, BinMap)) -> Item
renderItem (label, (c, bMap)) =
    Item { name        = label
         , x           = Map.elems bMap
         , y           = Map.keys bMap
         , type_       = Bar
         , orientation = Horizontal
         , marker      = Marker { color = c
                                , width = 1
                                }
         }

plottify :: ItemMap -> Plot
plottify = Plot . renderItems

randomColor :: IO Color
randomColor = RGB <$> r <*> g <*> b
  where
    r = randomRIO (0, 255)
    g = randomRIO (0, 255)
    b = randomRIO (0, 255)

{-
  JSON serializers.
-}

instance ToJSON Item where
  toJSON Item {..} =
    object [ "x"           .= x
           , "y"           .= y
           , "name"        .= name
           , "type"        .= type_
           , "orientation" .= orientation
           , "marker"      .= marker
           ]

instance ToJSON Marker where
  toJSON Marker {..} =
    object [ "color" .= color
           , "width" .= width
           ]

instance ToJSON Color where
  toJSON (RGB r g b) =
    let c = printf "rgba(%d, %d, %d, 0.4)" r g b
    in String (Text.pack c)

instance ToJSON Orientation where
  toJSON Horizontal = String "h"

instance ToJSON Barmode where
  toJSON Stack = String "stack"

instance ToJSON Plot where
  toJSON Plot {..} =
    object [ "data"   .= data_
           , "layout" .= object["barmode" .= Stack]
           ]
