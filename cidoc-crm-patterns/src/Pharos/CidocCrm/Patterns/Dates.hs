module Pharos.CidocCrm.Patterns.Dates where

import qualified Data.Text as T
import CommonImports


createDateLabel :: T.Text -> T.Text -> T.Text
createDateLabel earliestStr latestStr =
  if earliestStr == latestStr
  then earliestStr  -- If both years are the same, use just one year
  else earliestStr <> "-" <> latestStr