{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Types
  ( ResourceKey
  , ResourceMap
  , Payload
  , Context (..)
  , Entry (..)
  , Registration (..)
  , RegistryDescr (..)
  , newContext
  ) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Lazy (Map)
import Data.Text (Text)

import qualified Data.Map.Lazy as Map

type ResourceKey = Text
type ResourceMap = Map ResourceKey Entry
type Payload     = ByteString

data Context = Context
    { resourceMap :: TVar ResourceMap }

data Entry = Entry
    { description :: !Text
    , _type       :: !Text
    , payload     :: !(Maybe Payload)
    }

data Registration = Registration
    { descrRegistration :: !Text
    , typeRegistration  :: !Text
    }

data RegistryDescr = RegistryDescr
    { descrRegistryDescr :: !Text
    , typeRegistryDescr  :: !Text
    , link               :: !Text
    }

newContext :: IO Context
newContext = Context <$> newTVarIO Map.empty

instance FromJSON Registration where
  parseJSON (Object o) =
    Registration <$> o .: "description"
                 <*> o .: "type"
  parseJSON invalid    = typeMismatch "Registration" invalid

instance ToJSON RegistryDescr where
  toJSON RegistryDescr {..} =
    object [ "description" .= descrRegistryDescr
           , "type"        .= typeRegistryDescr
           , "link"        .= link
           ]
