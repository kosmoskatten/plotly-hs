{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Server
  ( server
  ) where

import Network.Hive

import Plotly.JSON (RegistryEntry (..))
import Types (Context, Entry (..), listEntries)

server :: Context -> Hive ()
server context = do
  match GET </> "rest" </> "plot" <!> None ==> listPlots context

listPlots :: Context -> Handler HandlerResponse
listPlots context = do
  entries <- liftIO $ listEntries context
  respondJSON Ok $ map toRegEntry entries
    where
      toRegEntry :: Entry -> RegistryEntry
      toRegEntry Entry {..} =
        RegistryEntry { description_regEntry = description_entry
                      , type_regEntry        = type_entry
                      , link                 = link_entry
                      }
