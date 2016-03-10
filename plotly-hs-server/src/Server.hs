{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Server
  ( server
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Text (Text)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Hive

import qualified Data.ByteString.Lazy.Char8 as LBS

import Plotly.JSON (RegistryEntry (..), Registration (..))
import Types ( Context (..)
             , Entry (..)
             , listRegEntries
             , insertNewEntry
             , readEntry
             , updateEntry
             )

server :: Context -> Hive ()
server context = do
  -- Redirect to index.html if accessing the service root.
  match GET <!> None
        ==> redirectTo "index.html"

  -- List the already registered plots.
  match GET </> "rest" </> "plot" <!> None
        ==> listPlots context

  -- Read the specified plot.
  match GET </> "rest" </> "plot" </:> "plotKey" <!> None
        ==> readPlot context

  -- Create a new - empty - plot given the specified registration details.
  match POST </> "rest" </> "plot" <!> None
        ==> (createPlot context =<< bodyJSON)

  -- Update a plot with new graphics data.
  match PUT </> "rest" </> "plot" </:> "plotKey" <!> None
        ==> (updatePlot context =<< bodyByteString)

  -- Fall back http case, try serving a static file.
  matchAll ==> serveDirectory (siteDir context)

  -- Serve a plot as a websocket service.
  webSocket </> "rest" </> "plot" </:> "plotKey"
            ~~> dataService

{-
  REST endpoints.
-}

listPlots :: Context -> Handler HandlerResponse
listPlots context = do
  entries <- liftIO $ listRegEntries context
  respondJSON Ok entries

readPlot :: Context -> Handler HandlerResponse
readPlot context = do
  plotKey    <- capture "plotKey"
  maybeEntry <- liftIO $ readEntry context plotKey
  case maybeEntry of
    Just entry -> respondByteString Ok "application/json" $ plot entry
    Nothing    -> respondText NotFound "Resource not found"

createPlot :: Context -> Registration -> Handler HandlerResponse
createPlot context Registration {..} = do
  uuid <- toText <$> liftIO nextRandom
  let regEntry' =
        RegistryEntry { description_regEntry = description_reg
                      , type_regEntry        = type_reg
                      , link                 = mkLink uuid
                      }
      entry = Entry { regEntry = regEntry'
                    , plot     = "{\"data\":[], \"layout\":{}}"
                    }
  liftIO $ insertNewEntry context uuid entry
  respondJSON Created regEntry'

updatePlot :: Context -> LBS.ByteString -> Handler HandlerResponse
updatePlot context obj = do
  plotKey <- capture "plotKey"
  result  <- liftIO $ updateEntry context plotKey obj
  if result then respondText Ok "Entry updated"
            else respondText NotFound "Entry not found"

{-
  WebSocket endpoint.
-}

-- TODO: Check that the entry exist, otherwise just reject.
dataService :: Server ()
dataService = acceptRequest connectedDataService

connectedDataService :: ConnectedServer ()
connectedDataService = do
  forkPingThread 20
  forever $ do
    sendTextMessage ("Hello" :: Text)
    liftIO $ threadDelay 1000000

{-
  Helpers.
-}

mkLink :: Text -> Text
mkLink uuid = "/rest/plot/" `mappend` uuid
