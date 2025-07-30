{-# LANGUAGE GADTs #-} -- Keep GADTs if needed for Class_ type literal

module Main where

import AppUtils (AppConfig(..), runApp)
import CidocCRM (Class_(E22_))
import qualified Mappings.Mappings.Photo.Photo as ZeriPhotoMap
import qualified Mappings.Mappings.Work.WorkMappings as ZeriWorkMap

zeriPhotoConfig :: AppConfig 'E22_
zeriPhotoConfig = AppConfig
    { appMapping         = ZeriPhotoMap.photoMapping
    , appBaseUri         = ZeriWorkMap.zeriBaseUri
    , appVocabularyItems = pure []
    }

main :: IO ()
main = runApp zeriPhotoConfig
