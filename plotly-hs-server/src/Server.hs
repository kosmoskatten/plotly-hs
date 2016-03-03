{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Server
  ( server
  ) where

import Data.Text (Text)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Hive

import Plotly.JSON (RegistryEntry (..), Registration (..))
import Types ( Context
             , Entry (..)
             , listEntries
             , createEntry
             , readEntry
             )

server :: Context -> Hive ()
server context = do
  match GET </> "rest" </> "plot" <!> None
        ==> listPlots context
  match GET </> "rest" </> "plot" </:> "plotKey" <!> None
        ==> readPlot context
  match POST </> "rest" </> "plot" <!> None
        ==> (createPlot context =<< bodyJSON)

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

readPlot :: Context -> Handler HandlerResponse
readPlot context = do
  plotKey    <- capture "plotKey"
  maybeEntry <- liftIO $ readEntry context plotKey
  case maybeEntry of
    Just entry -> respondJSON Ok $ plot_entry entry
    Nothing    -> respondText NotFound "Resource not found"

createPlot :: Context -> Registration -> Handler HandlerResponse
createPlot context Registration {..} = do
  uuid <- toText <$> liftIO nextRandom
  let entry = Entry { description_entry = description_reg
                    , type_entry        = type_reg
                    , link_entry        = mkLink uuid
                    , plot_entry        = plot_reg
                    }
      reply = RegistryEntry { description_regEntry = description_reg
                            , type_regEntry        = type_reg
                            , link                 = mkLink uuid
                            }
  liftIO $ createEntry context uuid entry
  respondJSON Created reply

mkLink :: Text -> Text
mkLink uuid = "/rest/plot/" `mappend` uuid
