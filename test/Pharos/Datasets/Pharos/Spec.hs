module Pharos.Datasets.Pharos.Spec (spec) where

import Test.Hspec
import qualified Pharos.Datasets.Pharos.PhotographerSpec as PhotographerSpec

spec :: Spec
spec = describe "Photographer" PhotographerSpec.spec
