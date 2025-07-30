module Main (main) where

import AppUtils (AppConfig(..), runApp)
import CidocCRM (Class_(E53_))
import MidasGeo.Mappings.Geo (geoMapping)

-- | Configuration specific to the Midas-Geo dataset.
midasGeoConfig :: AppConfig 'E53_
midasGeoConfig = AppConfig
    { appMapping         = geoMapping
    , appBaseUri         = Just "https://artresearch.net/resource/midas/"
    , appVocabularyItems = return []
    }

-- | Main entry point for the Midas-Geo dataset processor.
main :: IO ()
main = runApp midasGeoConfig
