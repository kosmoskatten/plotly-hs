module Plotly.REST
  ( createPlot
  , updatePlot
  ) where

import Data.Aeson (ToJSON, decode', toJSON)
import Control.Lens
import Control.Monad (void)
import Network.Wreq

import qualified Data.Text as Text

import Plotly.JSON

createPlot :: String -> Registration -> IO (Maybe RegistryEntry)
createPlot host reg = do
  r <- post (host ++ "/rest/plot") (toJSON reg)
  return $ decode' (r ^. responseBody)

updatePlot :: ToJSON a => String -> RegistryEntry -> a -> IO ()
updatePlot host reg plot = do
  let url = host ++ (Text.unpack $ link reg)
  void $ put url (toJSON plot)

