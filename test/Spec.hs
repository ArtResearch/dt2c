import Test.Hspec

import qualified Pharos.Datasets.Frick.Spec as FrickSpec
import qualified Pharos.Datasets.Midas.Spec as MidasSpec
import qualified Pharos.Datasets.Pharos.Spec as PharosSpec
import qualified Pharos.Datasets.Pmc.Spec as PmcSpec
import qualified Pharos.Datasets.Zeri.Spec as ZeriSpec
import qualified Pharos.DSL.Spec as DSLSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DSL"    DSLSpec.spec
  describe "Frick"  FrickSpec.spec
  describe "Midas"  MidasSpec.spec
  describe "Pharos" PharosSpec.spec
  describe "Pmc"    PmcSpec.spec
  describe "Zeri"   ZeriSpec.spec
