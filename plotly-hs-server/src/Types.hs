module Types
  ( PlotKey
  , PlotMap
  , Context (..)
  , Entry (..)
  , newContext
  , listRegEntries
  , insertNewEntry
  , readEntry
  , updateEntry
  ) where

import Control.Concurrent.STM ( TVar
                              , atomically
                              , newTVarIO
                              , readTVar
                              , readTVarIO
                              , modifyTVar
                              , writeTVar
                              )
import Data.Map.Lazy (Map)
import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Lazy as Map

import Plotly.JSON (RegistryEntry (..))

type PlotKey = Text
type PlotMap = Map PlotKey Entry

-- | Server context with the map of all plots.
data Context = Context
    { siteDir :: !FilePath
    , plotMap :: TVar PlotMap
    }

-- | An entry for one plot.
data Entry = Entry
    { regEntry :: !RegistryEntry
    , plot     :: !LBS.ByteString
    }

-- | Create a new context, with the given site dir and an empty plot map.
newContext :: FilePath -> IO Context
newContext dir = Context dir <$> newTVarIO Map.empty

-- | Get all registry entries from the context.
listRegEntries :: Context -> IO [RegistryEntry]
listRegEntries context =
  (map regEntry . Map.elems) <$> readTVarIO (plotMap context)

insertNewEntry :: Context -> PlotKey -> Entry -> IO ()
insertNewEntry context key entry =
  atomically $ modifyTVar (plotMap context) (Map.insert key entry)

readEntry :: Context -> PlotKey -> IO (Maybe Entry)
readEntry context plotKey = do
  plotMap' <- readTVarIO $ plotMap context
  return $ Map.lookup plotKey plotMap'

updateEntry :: Context -> PlotKey -> LBS.ByteString -> IO Bool
updateEntry context plotKey body =
  atomically $ do
    plotMap' <- readTVar (plotMap context)
    case Map.lookup plotKey plotMap' of
      Just entry -> do
        let entry' = entry { plot = body }
        writeTVar (plotMap context) $ Map.insert plotKey entry' plotMap'
        return True
      Nothing -> return False
