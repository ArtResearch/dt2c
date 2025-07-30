module Main where

import AppUtils (AppConfig(..), runApp)
import Midas.Mappings.Work
import Midas.Mappings.Vocabulary

hertzianaConfig = AppConfig
    { appMapping         = workMapping "hertziana"
    , appBaseUri         = midasBaseUri
    , appVocabularyItems = midasVocabularyItems
    }

main :: IO ()
main = runApp hertzianaConfig
