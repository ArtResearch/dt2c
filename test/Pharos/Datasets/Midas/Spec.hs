module Pharos.Datasets.Midas.Spec (spec) where

import Test.Hspec
import qualified Pharos.Datasets.Midas.MidasMappingsSpec as MappingsSpec
import qualified Pharos.Datasets.Midas.TitlesSpec as TitlesSpec
import qualified Pharos.Datasets.Midas.RepositorySpec as RepositorySpec
import qualified Pharos.Datasets.Midas.LocationSpec as LocationSpec
import qualified Pharos.Datasets.Midas.GeoSpec as GeoSpec
import qualified Pharos.Datasets.Midas.PhotoSpec as PhotoSpec
import qualified Pharos.Datasets.Midas.ArtistSpec as ArtistSpec
import qualified Pharos.Datasets.Midas.DateParserSpec as DateParserSpec
import qualified Pharos.Datasets.Midas.SubjectsSpec as SubjectsSpec
import qualified Pharos.Datasets.Midas.TypesSpec as TypesSpec
import qualified Pharos.Datasets.Midas.MaterialSpec as MaterialSpec
import qualified Pharos.Datasets.Midas.TechniqueSpec as TechniqueSpec
import qualified Pharos.Datasets.Midas.IdentifiersSpec as IdentifiersSpec

spec :: Spec
spec = do
  describe "Mappings" MappingsSpec.spec
  describe "Identifiers" IdentifiersSpec.spec
  describe "Titles" TitlesSpec.spec
  describe "Repository" RepositorySpec.spec
  describe "Location" LocationSpec.spec
  describe "Geo" GeoSpec.spec
  describe "Photo" PhotoSpec.spec
  describe "Artist" ArtistSpec.spec
  describe "DateParser" DateParserSpec.spec
  describe "Subjects" SubjectsSpec.spec
  describe "Types" TypesSpec.spec
  describe "Material" MaterialSpec.spec
  describe "Technique" TechniqueSpec.spec
