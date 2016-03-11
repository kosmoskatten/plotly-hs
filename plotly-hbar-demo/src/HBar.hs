{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HBar where

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Text (Text)

import Plotly.JSON
import Plotly.REST

import HorizontalBar

newtype HBar a = HBar { extrState :: StateT Context IO a }
  deriving (Functor, Applicative, Monad, MonadState Context, MonadIO)

data Context = Context
    { itemMap  :: !ItemMap
    , regEntry :: !RegistryEntry
    , theHost  :: !String
    }

-- | Run a new Horizontal bar data producing application. Will create a
-- new plot.
runHBar :: String -> Text -> HBar a -> IO a
runHBar host descr act = do
  let reg = Registration
        { description_reg = descr
        , type_reg        = Bar
        }
  entry <- fromJust <$> createPlot host reg
  let context = Context
        { itemMap  = emptyItemMap
        , regEntry = entry
        , theHost  = host
        }
  evalStateT (extrState act) context

commit :: HBar ()
commit = do
  context <- get
  let plottified = plottify $ itemMap context
  liftIO $ updatePlot (theHost context) (regEntry context) plottified

addItemWithCategory :: Text -> Text -> Float -> HBar ()
addItemWithCategory item category value = do
  context <- get
  im      <- liftIO $ addItemBin item category value (itemMap context)
  put $ context { itemMap = im }

