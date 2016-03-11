{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HorizontalBar
  ( ItemMap
  , Color
  , Plot (..)
  , emptyItemMap
  , addItemBin
  , renderItems
  , plottify
  , mkColorSelector
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.IORef
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

addItemBin :: ItemLabel -> BinLabel -> Float -> IO Color -> ItemMap
           -> IO ItemMap
addItemBin iLabel bLabel value newColor iMap =
  case Map.lookup iLabel iMap of
    Just (c, m)  ->
      return $ Map.insert iLabel (c, (addBinValue bLabel value m)) iMap
    Nothing      -> do
      c <- newColor
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

mkColorSelector :: IO (IO Color)
mkColorSelector = do
  cRef <- newIORef colors
  return $ selectColor cRef

randomColor :: IO Color
randomColor = RGB <$> r <*> g <*> b
  where
    r = randomRIO (0, 255)
    g = randomRIO (0, 255)
    b = randomRIO (0, 255)

selectColor :: IORef [Color] -> IO Color
selectColor cRef = do
  xs <- readIORef cRef
  if null xs then randomColor
             else do
               let y:ys = xs
               writeIORef cRef ys
               return y

colors :: [Color]
colors = [ {-RGB 255 0 0
         , RGB 255 127 0
         , RGB 255 255 0
         , RGB 0 255 0
         , RGB 0 0 255
         , RGB 75 0 130
         , RGB 143 0 255-}

           RGB 164 0 0
         , RGB 143 89 2
         , RGB 78 154 6
         , RGB 32 74 135
         , RGB 196 160 0
         , RGB 206 92 0
         , RGB 92 53 102
         , RGB 46 52 0
         ]

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
    let c = printf "rgba(%d, %d, %d, 0.7)" r g b
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
