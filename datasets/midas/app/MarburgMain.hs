module Main where

import AppUtils (AppConfig(..), runApp)
import Midas.Mappings.Work
import Midas.Mappings.Vocabulary

marburgConfig = AppConfig
    { appMapping         = workMapping "marburg"
    , appBaseUri         = midasBaseUri
    , appVocabularyItems = midasVocabularyItems
    }

main :: IO ()
main = runApp marburgConfig
