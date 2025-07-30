module Main where

import AppUtils (AppConfig(..), runApp)
import CidocCRM (Class_(E22_)) -- Assuming E22 for now, adjust if needed
import Pmc.Mappings.Work (workMapping, pmcBaseUri)
import qualified Pmc.Mappings.Vocabulary as PmcVocab -- Import the specific vocabulary module

-- | Configuration specific to the PMC dataset.
pmcConfig :: AppConfig 'E22_ -- Adjust Class_ if needed
pmcConfig = AppConfig
    { appMapping         =  workMapping
    , appBaseUri         = pmcBaseUri
    , appVocabularyItems = PmcVocab.pmcVocabularyItems -- Use the specific IO action
    }

-- | Main entry point for the PMC dataset processor.
main :: IO ()
main = runApp pmcConfig
