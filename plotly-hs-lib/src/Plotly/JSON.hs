{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Plotly.JSON
  ( Registration (..)
  , RegistryEntry (..)
  , Plot (..)
  , Data (..)
  , Layout (..)
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
    , plot_reg        :: !Plot
    } deriving Show

-- | Registry entry for a registered plot. Response from the server.
data RegistryEntry = RegistryEntry
    { description_regEntry :: !Text
    , type_regEntry        :: !Type
    , link                 :: !Text
    } deriving Show

-- | Plotly container record.
data Plot = Plot
    { data_  :: ![Data]
    , layout :: !Layout
    } deriving Show

-- | Plotly data.
data Data = Data
    { values :: !(Maybe [Float])
    , labels :: !(Maybe [Text])
    , type_  :: !Type
    } deriving Show

-- | Plotly layout.
data Layout = Layout
    { height :: !(Maybe Int)
    , width  :: !(Maybe Int)
    , title  :: !Text
    } deriving Show

-- | Enumeration of the supported types.
data Type = Pie
    deriving Show

-- Aeson instances for Registration.
instance FromJSON Registration where
  parseJSON (Object o) =
    Registration <$> o .: "description"
                 <*> o .: "type"
                 <*> o .: "plot"
  parseJSON invalid    = typeMismatch "Registration" invalid

instance ToJSON Registration where
  toJSON Registration {..} =
    object [ "description" .= description_reg
           , "type"        .= type_reg
           , "plot"        .= plot_reg
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

-- Aeson instances for Plot.
instance FromJSON Plot where
  parseJSON (Object o) =
    Plot <$> o .: "data"
         <*> o .: "layout"
  parseJSON invalid    = typeMismatch "Plot" invalid

instance ToJSON Plot where
  toJSON Plot {..} =
    object [ "data"   .= data_
           , "layout" .= layout
           ]

-- Aeson instances for Data.
instance FromJSON Data where
  parseJSON (Object o) =
    Data <$> o .:? "values"
         <*> o .:? "labels"
         <*> o .:  "type"
  parseJSON invalid    = typeMismatch "Data" invalid

instance ToJSON Data where
  toJSON Data {..} =
    object [ "values" .= values
           , "labels" .= labels
           , "type"   .= type_
           ]

-- Aeson instances for Layout.
instance FromJSON Layout where
  parseJSON (Object o) =
    Layout <$> o .:? "height"
           <*> o .:? "width"
           <*> o .: "title"
  parseJSON invalid    = typeMismatch "Layout" invalid

instance ToJSON Layout where
  toJSON Layout {..} =
    object [ "height" .= height
           , "width"  .= width
           , "title"  .= title
           ]

-- Aeson instances for Type.
instance FromJSON Type where
  parseJSON (String "pie") = return Pie
  parseJSON invalid        = typeMismatch "Type" invalid

instance ToJSON Type where
  toJSON Pie = String "pie"
