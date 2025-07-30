module Pharos.Datasets.Pmc.Spec (spec) where

import Test.Hspec
import qualified Pharos.Datasets.Pmc.CatalogingSpec as CatalogingSpec
import qualified Pharos.Datasets.Pmc.IdentifiersSpec as IdentifiersSpec
import qualified Pharos.Datasets.Pmc.PmcMappingsSpec as MappingsSpec
import qualified Pharos.Datasets.Pmc.TitlesSpec as TitlesSpec
import qualified Pharos.Datasets.Pmc.RepositorySpec as RepositorySpec
import qualified Pharos.Datasets.Pmc.PhotoSpec as PhotoSpec
import qualified Pharos.Datasets.Pmc.ArtistSpec as ArtistSpec
import qualified Pharos.Datasets.Pmc.SubjectsSpec as SubjectsSpec
import qualified Pharos.Datasets.Pmc.TypesSpec as TypesSpec
import qualified Pharos.Datasets.Pmc.MaterialSpec as MaterialSpec
import qualified Pharos.Datasets.Pmc.TechniqueSpec as TechniqueSpec

spec :: Spec
spec = do
  describe "Cataloging"  CatalogingSpec.spec
  describe "Identifiers" IdentifiersSpec.spec
  describe "Mappings"    MappingsSpec.spec
  describe "Titles"      TitlesSpec.spec
  describe "Repository"  RepositorySpec.spec
  describe "Photo"       PhotoSpec.spec
  describe "Production"  ArtistSpec.spec
  describe "Subjects"    SubjectsSpec.spec
  describe "Types"       TypesSpec.spec
  describe "Material"    MaterialSpec.spec
  describe "Technique"   TechniqueSpec.spec
