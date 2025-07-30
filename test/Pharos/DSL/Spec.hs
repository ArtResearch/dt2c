module Pharos.DSL.Spec (spec) where

import Test.Hspec

import qualified Pharos.DSL.XMLSpec as XMLSpec
import qualified Pharos.DSL.GeneratorSpec as GeneratorSpec

spec :: Spec
spec = do
  describe "XML"     XMLSpec.spec
  describe "Generators"     GeneratorSpec.spec
