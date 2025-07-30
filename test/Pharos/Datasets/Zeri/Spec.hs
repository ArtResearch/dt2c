module Pharos.Datasets.Zeri.Spec (spec) where

import Test.Hspec
import qualified Pharos.Datasets.Zeri.TitlesSpec as TitlesSpec
import qualified Pharos.Datasets.Zeri.ArtistSpec as ArtistSpec
import qualified Pharos.Datasets.Zeri.ZeriMaterialTechniqueSpec as MaterialTechniqueSpec
import qualified Pharos.Datasets.Zeri.LocationSpec as LocationSpec
import qualified Pharos.Datasets.Zeri.PhotoSpec as PhotoSpec
import qualified Pharos.Datasets.Zeri.SubjectsSpec as SubjectsSpec
import qualified Pharos.Datasets.Zeri.TypesSpec as TypesSpec
import qualified Pharos.Datasets.Zeri.MaterialSpec as MaterialSpec
import qualified Pharos.Datasets.Zeri.TechniqueSpec as TechniqueSpec

spec :: Spec
spec = do
  describe "Titles"             TitlesSpec.spec
  describe "Artist"             ArtistSpec.spec
  describe "Material/Technique" MaterialTechniqueSpec.spec
  describe "Location"           LocationSpec.spec
  describe "Photo"              PhotoSpec.spec
  describe "Subjects"           SubjectsSpec.spec
  describe "Types"              TypesSpec.spec
  describe "Material"           MaterialSpec.spec
  describe "Technique"          TechniqueSpec.spec
