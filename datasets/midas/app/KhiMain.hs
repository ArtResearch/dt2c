module Main where

import AppUtils (AppConfig(..), runApp)
import Midas.Mappings.Work
import Midas.Mappings.Vocabulary

khiConfig = AppConfig
    { appMapping         = workMapping "khi"
    , appBaseUri         = midasBaseUri
    , appVocabularyItems = midasVocabularyItems
    }

main :: IO ()
main = runApp khiConfig
