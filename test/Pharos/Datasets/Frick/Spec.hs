module Pharos.Datasets.Frick.Spec (spec) where

import Test.Hspec

import qualified Pharos.Datasets.Frick.TitlesSpec as TitlesSpec
import qualified Pharos.Datasets.Frick.IdentifiersSpec as IdentifiersSpec
import qualified Pharos.Datasets.Frick.RepositoriesSpec as RepositoriesSpec
import qualified Pharos.Datasets.Frick.PhotoSpec as PhotoSpec
import qualified Pharos.Datasets.Frick.ArtistSpec as ArtistSpec
import qualified Pharos.Datasets.Frick.SubjectsSpec as SubjectsSpec
import qualified Pharos.Datasets.Frick.TypesSpec as TypesSpec
import qualified Pharos.Datasets.Frick.MaterialSpec as MaterialSpec
import qualified Pharos.Datasets.Frick.TechniqueSpec as TechniqueSpec

spec :: Spec
spec = do
  describe "Titles"     TitlesSpec.spec
  describe "Identifiers"     IdentifiersSpec.spec
  describe "Repositories"  RepositoriesSpec.spec
  describe "Photo"        PhotoSpec.spec
  describe "Artist" ArtistSpec.spec
  describe "Subjects" SubjectsSpec.spec
  describe "Types" TypesSpec.spec
  describe "Material" MaterialSpec.spec
  describe "Technique" TechniqueSpec.spec
