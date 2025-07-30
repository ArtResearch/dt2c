module Main where

import AppUtils (AppConfig(..), runApp)
import Mappings.Photographers (actorMapping, pharosBaseUri)

pharosConfig = AppConfig
    { appMapping         = actorMapping
    , appBaseUri         = pharosBaseUri
    , appVocabularyItems = pure []
    }

main :: IO ()
main = runApp pharosConfig
