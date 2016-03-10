{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Server
  ( server
  ) where

import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Data.Text (Text)
import Network.Hive
import qualified Data.ByteString.Lazy.Char8 as LBS

import Plotly.JSON (Registration (..))
import Types ( Context (..)
             , Entry (..)
             , PlotChan
             , newEntry
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
            ~~> dataService context

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
createPlot context registration = do
  (key, entry) <- liftIO $ newEntry registration
  liftIO $ insertNewEntry context key entry
  respondJSON Created $ regEntry entry

updatePlot :: Context -> LBS.ByteString -> Handler HandlerResponse
updatePlot context obj = do
  plotKey <- capture "plotKey"
  result  <- liftIO $ updateEntry context plotKey obj
  if result then respondText Ok "Entry updated"
            else respondText NotFound "Entry not found"

{-
  WebSocket endpoint.
-}

dataService :: Context -> Server ()
dataService context = do
  plotKey    <- capture "plotKey"
  maybeEntry <- liftIO $ readEntry context plotKey
  case maybeEntry of
    Just entry -> do
      chan <- liftIO $ atomically (cloneTChan $ plotChan entry)
      acceptRequest $ connectedDataService chan
    Nothing    -> rejectRequest "Cannot find plot"

connectedDataService :: PlotChan -> ConnectedServer ()
connectedDataService chan = do
  forkPingThread 20
  forever $ do
    msg <- liftIO (atomically $ readTChan chan)
    sendTextMessage msg
