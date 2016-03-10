{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Types
  ( PlotKey
  , PlotMap
  , PlotChan
  , Context (..)
  , Entry (..)
  , newContext
  , newEntry
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
import Control.Concurrent.STM.TChan
import Data.Map.Lazy (Map)
import Data.Text (Text)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Lazy as Map

import Plotly.JSON ( Registration (..)
                   , RegistryEntry (..)
                   )

type PlotKey  = Text
type PlotMap  = Map PlotKey Entry
type PlotChan = TChan LBS.ByteString

-- | Server context with the map of all plots.
data Context = Context
    { siteDir :: !FilePath
    , plotMap :: TVar PlotMap
    }

-- | An entry for one plot.
data Entry = Entry
    { regEntry :: !RegistryEntry
    , plot     :: !LBS.ByteString
    , plotChan :: !PlotChan
    }

-- | Create a new context, with the given site dir and an empty plot map.
newContext :: FilePath -> IO Context
newContext dir = Context dir <$> newTVarIO Map.empty

-- | Create a new entry from the given registration.
newEntry :: Registration -> IO (PlotKey, Entry)
newEntry Registration {..} = do
  let emptyPlot = "{\"data\":[], \"layout\":{}}"
  chan <- newTChanIO
  atomically $ writeTChan chan emptyPlot
  key  <- toText <$> nextRandom
  let regEntry' =
        RegistryEntry
          { description_regEntry = description_reg
          , type_regEntry        = type_reg
          , link                 = mkLink key
          }
      entry =
        Entry
          { regEntry = regEntry'
          , plot     = emptyPlot
          , plotChan = chan
          }
  return (key, entry)

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
        writeTChan (plotChan entry) body
        let entry' = entry { plot = body }
        writeTVar (plotMap context) $ Map.insert plotKey entry' plotMap'
        return True
      Nothing -> return False

mkLink :: Text -> Text
mkLink uuid = "/rest/plot/" `mappend` uuid
