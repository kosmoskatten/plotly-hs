module Types
  ( PlotKey
  , PlotMap
  , Context (..)
  , Entry (..)
  , newContext
  , listEntries
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Data.Map.Lazy (Map)
import Data.Text (Text)

import qualified Data.Map.Lazy as Map

import Plotly.JSON (Type, Plot)

type PlotKey = Text
type PlotMap = Map PlotKey Entry

-- | Server context with the map of all plots.
data Context = Context
    { plotMap :: TVar PlotMap }

-- | An entry for one plot.
data Entry = Entry
    { description_entry :: !Text
    , type_entry        :: !Type
    , link_entry        :: !Text
    , plot              :: !Plot
    }

-- | Create a new, empty, context.
newContext :: IO Context
newContext = Context <$> newTVarIO Map.empty

-- | Get all the entries from the context.
listEntries :: Context -> IO [Entry]
listEntries context = Map.elems <$> readTVarIO (plotMap context)

