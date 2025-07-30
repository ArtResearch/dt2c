module Main where

import AppUtils (AppConfig(..), runApp)
import Pharos.Datasets.Frick.Mappings.Work

frickConfig = AppConfig
    { appMapping         = workMapping
    , appBaseUri         = frickBaseUri
    , appVocabularyItems = pure []
    }

main :: IO ()
main = runApp frickConfig
