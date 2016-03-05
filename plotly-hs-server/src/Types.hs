module Types
  ( PlotKey
  , PlotMap
  , Context (..)
  , Entry (..)
  , newContext
  , listEntries
  , createEntry
  , readEntry
  ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , readTVarIO
                              , modifyTVar
                              )
import Data.Map.Lazy (Map)
import Data.Text (Text)

import qualified Data.Map.Lazy as Map

import Plotly.JSON (Type, Plot)

type PlotKey = Text
type PlotMap = Map PlotKey Entry

-- | Server context with the map of all plots.
data Context = Context
    { siteDir :: !FilePath
    , plotMap :: TVar PlotMap
    }

-- | An entry for one plot.
data Entry = Entry
    { description_entry :: !Text
    , type_entry        :: !Type
    , link_entry        :: !Text
    , plot_entry        :: !Plot
    }

-- | Create a new context, with the given site dir and an empty plot map.
newContext :: FilePath -> IO Context
newContext dir = Context dir <$> newTVarIO Map.empty

-- | Get all the entries from the context.
listEntries :: Context -> IO [Entry]
listEntries context = Map.elems <$> readTVarIO (plotMap context)

createEntry :: Context -> PlotKey -> Entry -> IO ()
createEntry context key entry =
  atomically $ modifyTVar (plotMap context) (Map.insert key entry)

readEntry :: Context -> PlotKey -> IO (Maybe Entry)
readEntry context plotKey = do
  plotMap' <- readTVarIO $ plotMap context
  return $ Map.lookup plotKey plotMap'
