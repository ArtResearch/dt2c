{-# LANGUAGE GADTs #-} -- Keep GADTs if needed for Class_ type literal

module Main where

import AppUtils (AppConfig(..), runApp)
import CidocCRM (Class_(E22_))
import qualified Mappings.Mappings.Work.WorkMappings as ZeriMap

-- | Configuration specific to the Zeri dataset.
zeriConfig :: AppConfig 'E22_
zeriConfig = AppConfig
    { appMapping         = ZeriMap.workMapping
    , appBaseUri         = ZeriMap.zeriBaseUri
    , appVocabularyItems = pure []
    }

-- | Main entry point for the Zeri dataset processor.
main :: IO ()
main = runApp zeriConfig
