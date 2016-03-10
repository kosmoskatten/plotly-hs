{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Plotly.JSON
  ( Registration (..)
  , RegistryEntry (..)
  , Type (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

-- | Registration record for the registration of a new plot using the
-- REST API.
data Registration = Registration
    { description_reg :: !Text
    , type_reg        :: !Type
    } deriving Show

-- | Registry entry for a registered plot. Response from the server.
data RegistryEntry = RegistryEntry
    { description_regEntry :: !Text
    , type_regEntry        :: !Type
    , link                 :: !Text
    } deriving Show

-- | Enumeration of the supported types.
data Type
    = Pie
    | Bar
    deriving Show

-- Aeson instances for Registration.
instance FromJSON Registration where
  parseJSON (Object o) =
    Registration <$> o .: "description"
                 <*> o .: "type"
  parseJSON invalid    = typeMismatch "Registration" invalid

instance ToJSON Registration where
  toJSON Registration {..} =
    object [ "description" .= description_reg
           , "type"        .= type_reg
           ]

-- Aeson instances for RegistryEntry.
instance FromJSON RegistryEntry where
  parseJSON (Object o) =
    RegistryEntry <$> o .: "description"
                  <*> o .: "type"
                  <*> o .: "link"
  parseJSON invalid    = typeMismatch "RegistryEntry" invalid

instance ToJSON RegistryEntry where
  toJSON RegistryEntry {..} =
    object [ "description" .= description_regEntry
           , "type"        .= type_regEntry
           , "link"        .= link
           ]

-- Aeson instances for Type.
instance FromJSON Type where
  parseJSON (String "pie") = return Pie
  parseJSON (String "bar") = return Bar
  parseJSON invalid        = typeMismatch "Type" invalid

instance ToJSON Type where
  toJSON Pie = String "pie"
  toJSON Bar = String "bar"
