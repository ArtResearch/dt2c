module Pharos.Datasets.Midas.DateParserSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Data.Either (isLeft)
import Midas.Mappings.Utils.DateParser (parseSingleUnitToRange, DateRangeResult(..))

spec :: Spec
spec = describe "Midas.Mappings.Utils.DateParser.parseSingleUnitToRange" $ do
  let p = T.pack
  let dr start end unc = Right $ DateRangeResult { rangeStart = p start, rangeEnd = p end, uncertain = unc }

  it "parses '1601/1700' (century range)" $ do
    parseSingleUnitToRange (p "1601/1700") `shouldBe` dr "1601-01-01T00:00:00Z" "1700-12-31T23:59:59Z" False

  it "parses 'um 1500' (around a year)" $ do
    parseSingleUnitToRange (p "um 1500") `shouldBe` dr "1498-01-01T00:00:00Z" "1502-12-31T23:59:59Z" False

  it "parses '1701/1800?' (uncertain century range)" $ do
    parseSingleUnitToRange (p "1701/1800?") `shouldBe` dr "1701-01-01T00:00:00Z" "1800-12-31T23:59:59Z" True

  it "parses '1894' (single year)" $ do
    parseSingleUnitToRange (p "1894") `shouldBe` dr "1894-01-01T00:00:00Z" "1894-12-31T23:59:59Z" False

  it "parses '1291ante/1185ante' (BCE range)" $ do
    parseSingleUnitToRange (p "1291ante/1185ante") `shouldBe` dr "-1291-01-01T00:00:00Z" "-1185-12-31T23:59:59Z" False

  it "parses '1235-1280' (year range with hyphen)" $ do
    parseSingleUnitToRange (p "1235-1280") `shouldBe` dr "1235-01-01T00:00:00Z" "1280-12-31T23:59:59Z" False

  it "parses '300ante/0' (BCE to CE year 0 range)" $ do
    parseSingleUnitToRange (p "300ante/0") `shouldBe` dr "-0300-01-01T00:00:00Z" "0000-12-31T23:59:59Z" False

  it "parses 'vor 1241' (before a year)" $ do
    parseSingleUnitToRange (p "vor 1241") `shouldBe` dr "1141-01-01T00:00:00Z" "1241-12-31T23:59:59Z" False

  it "parses '1950.12.30' (full date DD.MM.YYYY format)" $ do
    parseSingleUnitToRange (p "1950.12.30") `shouldBe` dr "1950-12-30T00:00:00Z" "1950-12-30T23:59:59Z" False

  it "parses '2015.05' (month.year format)" $ do
    parseSingleUnitToRange (p "2015.05") `shouldBe` dr "2015-05-01T00:00:00Z" "2015-05-31T23:59:59Z" False

  it "parses 'um 480ante' (around a BCE year)" $ do
    -- "um 480ante" means approx 480 BCE. Modifier 'um' is +/- 2 years.
    -- For BCE, 480+2 = 482 BCE (earlier), 480-2 = 478 BCE (later).
    -- So range is from 482 BCE to 478 BCE.
    parseSingleUnitToRange (p "um 480ante") `shouldBe` dr "-0482-01-01T00:00:00Z" "-0478-12-31T23:59:59Z" False

  it "parses '119' (year with less than 4 digits)" $ do
    parseSingleUnitToRange (p "119") `shouldBe` dr "0119-01-01T00:00:00Z" "0119-12-31T23:59:59Z" False

  it "parses '400ante/500' (BCE to CE range)" $ do
    parseSingleUnitToRange (p "400ante/500") `shouldBe` dr "-0400-01-01T00:00:00Z" "0500-12-31T23:59:59Z" False

  it "parses 'ca. 1950?' (circa a year, uncertain - from module example)" $ do
    parseSingleUnitToRange (p "ca. 1950?") `shouldBe` dr "1940-01-01T00:00:00Z" "1960-12-31T23:59:59Z" True

  it "parses 'ca.  1950 ?' (modifier and uncertainty with extra spaces)" $ do
    parseSingleUnitToRange (p "ca.  1950 ?") `shouldBe` dr "1940-01-01T00:00:00Z" "1960-12-31T23:59:59Z" True

  it "parses '22.09.1601/22.11.1602' (full date range DD.MM.YYYY/DD.MM.YYYY)" $ do
    parseSingleUnitToRange (p "22.09.1601/22.11.1602") `shouldBe` dr "1601-09-22T00:00:00Z" "1602-11-22T23:59:59Z" False

  it "returns Left for invalid date string 'invalid date'" $ do
    parseSingleUnitToRange (p "invalid date") `shouldSatisfy` isLeft

  it "returns Left for invalid date string '12.34.5678'" $ do
    parseSingleUnitToRange (p "12.34.5678") `shouldSatisfy` isLeft
    
  it "returns Left for invalid date string '1999.99'" $ do
    parseSingleUnitToRange (p "1999.99") `shouldSatisfy` isLeft

  -- New test cases for combined ranges and uncertainty
  it "parses 'vor 1586-1601/1615' (modifier on range, then slash)" $ do
    -- "vor 1586-1601" -> start: (1586-100)=1486, end: 1601. Range is 1486-01-01 to 1601-12-31
    -- "/1615" -> end: 1615. Range is 1615-01-01 to 1615-12-31
    -- Combined: start of (vor 1586-1601) and end of (1615)
    parseSingleUnitToRange (p "vor 1586-1601/1615") `shouldBe` dr "1486-01-01T00:00:00Z" "1615-12-31T23:59:59Z" False

  it "returns Left for '1901.02.29' (invalid leap day)" $ do
    parseSingleUnitToRange (p "1901.02.29") `shouldSatisfy` isLeft

  it "parses '1554-1555/1560-1590' (range / range)" $ do
    -- Left part "1554-1555": start 1554-01-01, end 1555-12-31
    -- Right part "1560-1590": start 1560-01-01, end 1590-12-31
    -- Combined: start of left, end of right
    parseSingleUnitToRange (p "1554-1555/1560-1590") `shouldBe` dr "1554-01-01T00:00:00Z" "1590-12-31T23:59:59Z" False

  it "parses '1429?/1434' (piece-level uncertainty in first part of slash)" $ do
    parseSingleUnitToRange (p "1429?/1434") `shouldBe` dr "1429-01-01T00:00:00Z" "1434-12-31T23:59:59Z" True

  it "parses '1429/1434?' (piece-level uncertainty in second part of slash)" $ do
    parseSingleUnitToRange (p "1429/1434?") `shouldBe` dr "1429-01-01T00:00:00Z" "1434-12-31T23:59:59Z" True

  it "parses '1429?/1434?' (piece-level uncertainty in both parts of slash)" $ do
    parseSingleUnitToRange (p "1429?/1434?") `shouldBe` dr "1429-01-01T00:00:00Z" "1434-12-31T23:59:59Z" True

  it "parses '1429-1430?/1434' (piece-level uncertainty in dash range)" $ do
    parseSingleUnitToRange (p "1429-1430?/1434") `shouldBe` dr "1429-01-01T00:00:00Z" "1434-12-31T23:59:59Z" True
    
  it "parses '1500?' (single year with piece-level uncertainty)" $ do
    parseSingleUnitToRange (p "1500?") `shouldBe` dr "1500-01-01T00:00:00Z" "1500-12-31T23:59:59Z" True

  it "parses 'ca. 1500?' (modifier with piece-level uncertainty)" $ do
    parseSingleUnitToRange (p "ca. 1500?") `shouldBe` dr "1490-01-01T00:00:00Z" "1510-12-31T23:59:59Z" True

  it "parses '1429/1434 ?' (overall uncertainty with trailing '?')" $ do
    parseSingleUnitToRange (p "1429/1434 ?") `shouldBe` dr "1429-01-01T00:00:00Z" "1434-12-31T23:59:59Z" True
    
  it "parses '1429?/1434 ?' (piece-level and overall uncertainty)" $ do
    parseSingleUnitToRange (p "1429?/1434 ?") `shouldBe` dr "1429-01-01T00:00:00Z" "1434-12-31T23:59:59Z" True

  it "parses '1430/ca.1440' (year / circa year)" $ do
    parseSingleUnitToRange (p "1430/ca.1440") `shouldBe` dr "1430-01-01T00:00:00Z" "1450-12-31T23:59:59Z" False

  it "parses '1320/1329-1330/1339' (interpreted as (1320/1329)-(1330/1339))" $ do
    parseSingleUnitToRange (p "1320/1329-1330/1339") `shouldBe` dr "1320-01-01T00:00:00Z" "1339-12-31T23:59:59Z" False

  -- New test cases for complex modifiers, case-insensitivity, and new uncertainty formats

  it "parses 'um/nach 1763' (complex modifier um/nach)" $ do
    parseSingleUnitToRange (p "um/nach 1763") `shouldBe` dr "1761-01-01T00:00:00Z" "1863-12-31T23:59:59Z" False

  it "parses 'vor/um 1700' (complex modifier vor/um)" $ do
    parseSingleUnitToRange (p "vor/um 1700") `shouldBe` dr "1600-01-01T00:00:00Z" "1702-12-31T23:59:59Z" False

  it "parses 'Um 1491' (case-insensitive modifier 'Um')" $ do
    parseSingleUnitToRange (p "Um 1491") `shouldBe` dr "1489-01-01T00:00:00Z" "1493-12-31T23:59:59Z" False

  it "parses 'CA. 1900' (case-insensitive modifier 'CA.')" $ do
    parseSingleUnitToRange (p "CA. 1900") `shouldBe` dr "1890-01-01T00:00:00Z" "1910-12-31T23:59:59Z" False

  it "parses 'UM/NACH 1763' (case-insensitive complex modifier)" $ do
    parseSingleUnitToRange (p "UM/NACH 1763") `shouldBe` dr "1761-01-01T00:00:00Z" "1863-12-31T23:59:59Z" False

  it "parses 'um 1108 (?)' (uncertainty with parentheses and spaces)" $ do
    parseSingleUnitToRange (p "um 1108 (?)") `shouldBe` dr "1106-01-01T00:00:00Z" "1110-12-31T23:59:59Z" True

  it "parses '1530/1570[?]' (uncertainty with brackets on second piece)" $ do
    parseSingleUnitToRange (p "1530/1570[?]") `shouldBe` dr "1530-01-01T00:00:00Z" "1570-12-31T23:59:59Z" True

  it "parses '1530[?]/1570' (uncertainty with brackets on first piece)" $ do
    parseSingleUnitToRange (p "1530[?]/1570") `shouldBe` dr "1530-01-01T00:00:00Z" "1570-12-31T23:59:59Z" True
    
  it "parses '1530[ ? ]/1570' (uncertainty with brackets and spaces on first piece)" $ do
    parseSingleUnitToRange (p "1530[ ? ]/1570") `shouldBe` dr "1530-01-01T00:00:00Z" "1570-12-31T23:59:59Z" True

  it "parses '1475‐1477' (year range with U+2010 hyphen)" $ do -- Note: '‐' is U+2010 hyphen
    parseSingleUnitToRange (p "1475‐1477") `shouldBe` dr "1475-01-01T00:00:00Z" "1477-12-31T23:59:59Z" False
    
  it "parses 'um 1108 (?) ?' (piece-level and overall uncertainty)" $ do
    parseSingleUnitToRange (p "um 1108 (?) ?") `shouldBe` dr "1106-01-01T00:00:00Z" "1110-12-31T23:59:59Z" True
    
  it "parses '1530/1570[?] ?' (piece-level bracket uncertainty and overall uncertainty)" $ do
    parseSingleUnitToRange (p "1530/1570[?] ?") `shouldBe` dr "1530-01-01T00:00:00Z" "1570-12-31T23:59:59Z" True
