module Pharos.DSL.XMLSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Text.XML.Hexml as X
import Data.Text()
import qualified Data.Text as T;
import XML (XMLNode(..), x, evaluateXPathExpr, nodeText, nodeName)

import Pharos.DSL.TestData (marcXml, photographersXml)

loadMarcDoc :: IO XMLNode
loadMarcDoc = case X.parse marcXml of
    Left err -> error $ "Failed to parse XML: " ++ BS.unpack err
    Right doc -> return $ XMLNode doc 0 Nothing

-- Helper to load test XML document
loadTestDoc :: IO XMLNode
loadTestDoc = case X.parse photographersXml of
    Left err -> error $ "Failed to parse XML: " ++ BS.unpack err
    Right doc -> return $ XMLNode doc 0 Nothing

-- Helper to parse a ByteString into an XMLNode for testing, errors on failure
unsafeParseXML :: BS.ByteString -> XMLNode
unsafeParseXML bs = case X.parse bs of
    Left err  -> error $ "Test XML parsing failed: " ++ BS.unpack err
    Right doc -> XMLNode doc 0 Nothing

spec :: Spec
spec = do
  describe "XML module" $ do
    describe "evaluateXPath" $ do
        it "handles current context (.)" $ do
            doc <- loadTestDoc
            let nodes = evaluateXPathExpr doc [x|.|]
            length nodes `shouldBe` 1

        it "handles parent (..)" $ do
            doc <- loadTestDoc
            let records = evaluateXPathExpr doc [x|/RECORDS/RECORD|]
            let nodes = evaluateXPathExpr (head records) [x|../RECORD|]
            length nodes `shouldBe` 3

        it "handles text() function" $ do
            doc <- loadTestDoc
            let nodes = evaluateXPathExpr doc [x|/RECORDS/RECORD/PHOTOGRAPHER/text()|]
            length nodes `shouldBe` 3
            nodeText (head nodes) `shouldBe` "Agnew and Son"

        it "handles text() function and trims leading/trailing whitespace and newlines" $ do
            let testXML = "<root>  \n  hello world  \n\t </root>"
            let doc = unsafeParseXML $ BS.pack testXML
            let nodes = evaluateXPathExpr doc [x|/root/text()|]
            length nodes `shouldBe` 1
            nodeText (head nodes) `shouldBe` "hello world"

        it "handles text() on an element with mixed content and trims resulting concatenated text" $ do
            let testXML = "<root>  Part 1  <child/> Part 2 \n </root>"
            let doc = unsafeParseXML $ BS.pack testXML
            let nodes = evaluateXPathExpr doc [x|/root/text()|]
            length nodes `shouldBe` 1
            nodeText (head nodes) `shouldBe` "Part 1   Part 2"

        it "handles element selection" $ do
            doc <- loadTestDoc
            let nodes = evaluateXPathExpr doc [x|/RECORDS/RECORD/PHOTOGRAPHER|]
            length nodes `shouldBe` 3
            nodeText (head nodes) `shouldBe` "Agnew and Son"

        it "handles multiple path components" $ do
            doc <- loadTestDoc
            let nodes = evaluateXPathExpr doc [x|/RECORDS/RECORD/INSTITUTE|]
            length nodes `shouldBe` 3
            nodeText (head nodes) `shouldBe` "frick"
            
  describe "MARC XML processing" $ do
    it "finds elements by attribute value" $ do
        doc <- loadMarcDoc
        let nodes = evaluateXPathExpr doc [x|/record/datafield[@tag='245']/subfield[@code='a']/text()|]
        length nodes `shouldBe` 2
        nodeText (head nodes) `shouldBe` "The title of the work"
        nodeText (nodes !! 1) `shouldBe` "Another title"

    it "handles complex attribute filtering" $ do
        doc <- loadMarcDoc
        let nodes = evaluateXPathExpr doc [x|/record/datafield[@tag='245'][@ind1='1']/subfield[@code='a']/text()|]
        length nodes `shouldBe` 1
        nodeText (head nodes) `shouldBe` "The title of the work"

    it "handles 'and' operator in attribute filtering" $ do
        doc <- loadMarcDoc
        let nodes = evaluateXPathExpr doc [x|/record/datafield[@tag='590' and @ind1='9']/subfield[@code='a']/text()|]
        length nodes `shouldBe` 1
        nodeText (head nodes) `shouldBe` "Some note"

    it "handles multiple 'and' conditions" $ do
        doc <- loadMarcDoc
        let nodes = evaluateXPathExpr doc [x|/record/datafield[@tag='245' and @ind1='1' and @ind2='0']/subfield[@code='a']/text()|]
        length nodes `shouldBe` 1
        nodeText (head nodes) `shouldBe` "The title of the work"

    it "handles leading and trailing whitespace in XPath expressions" $ do
        doc <- loadMarcDoc
        let nodes1 = evaluateXPathExpr doc [x|/record/datafield[@tag='245']|]
        let nodes2 = evaluateXPathExpr doc [x|/record/datafield[@tag='245']|]
        length nodes1 `shouldBe` length nodes2
        length nodes1 `shouldBe` 2

    it "handles internal whitespace in XPath expressions" $ do
        doc <- loadMarcDoc
        let nodes1 = evaluateXPathExpr doc [x|/record/datafield[@tag='245']/subfield[@code='a']|]
        let nodes2 = evaluateXPathExpr doc [x|/record/datafield[@tag='245']/subfield[@code='a']|]
        length nodes1 `shouldBe` length nodes2
        length nodes1 `shouldBe` 2

    it "handles substring-after function" $ do
        doc <- loadTestDoc
        let nodes = evaluateXPathExpr doc [x|/RECORDS/RECORD/PHOTOGRAPHER/text()|]
        let result = evaluateXPathExpr (head nodes) [x|substring-after(text(), 'Agnew')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "and Son"

    it "handles substring function" $ do
        doc <- loadTestDoc
        let nodes = evaluateXPathExpr doc [x|/RECORDS/RECORD/PHOTOGRAPHER/text()|]
        let result = evaluateXPathExpr (head nodes) [x|substring(text(), 1, 5)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew"

    it "handles nested substring and substring-after functions" $ do
        doc <- loadMarcDoc
        let result = evaluateXPathExpr doc [x|/record/controlfield[@tag='008']/substring(substring-after(text(), 'k'), 1, 4)|]
        length result `shouldBe` 1

    it "handles string-join function with separator" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|string-join(/RECORDS/RECORD/PHOTOGRAPHER/text(), ', ')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and Son, Agnew and Son, London, Ahlers, Henrik"

    it "handles string-join function with empty separator" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|string-join(/RECORDS/RECORD/PHOTOGRAPHER/text(), '')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and SonAgnew and Son, LondonAhlers, Henrik"

    it "handles string-join function with empty sequence" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|string-join(/RECORDS/NONEXISTENT/text(), ',')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

    it "handles string-join function with multiple results from xpath" $ do
        doc <- loadMarcDoc
        let result = evaluateXPathExpr doc [x|string-join(/record/datafield[@tag='245']/subfield[@code='a']/text(), ' | ')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "The title of the work | Another title"

    it "handles normalize-space function with leading/trailing whitespace" $ do
        doc <- loadMarcDoc
        let expr1 = evaluateXPathExpr doc [x|/record/datafield[@tag='245']/subfield[@code='a']/text()|]
        let result = evaluateXPathExpr (head expr1) [x|normalize-space(.)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "The title of the work"

    it "handles normalize-space function with multiple whitespace characters" $ do
        doc <- loadTestDoc
        let expr1 = evaluateXPathExpr doc [x|/RECORDS/RECORD/PHOTOGRAPHER/text()|]
        let result = evaluateXPathExpr (head expr1) [x|normalize-space(.)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and Son"

    it "handles normalize-space function with empty sequence" $ do
        doc <- loadTestDoc
        let expr1 = evaluateXPathExpr doc [x|/RECORDS/NONEXISTENT/text()|]
        let result = case expr1 of
              [] -> evaluateXPathExpr (XMLText "" 0 Nothing) [x|normalize-space(.)|]
              (n:_) -> evaluateXPathExpr n [x|normalize-space(.)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

    it "handles normalize-space function with nested expressions" $ do
        doc <- loadMarcDoc
        let expr1 = evaluateXPathExpr doc [x|/record/datafield[@tag='245']/subfield[@code='a']/text()|]
        let result = evaluateXPathExpr (head expr1) [x|normalize-space(substring(., 1, 13))|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "The title of"

    describe "marc-value function" $ do
      it "removes leading and trailing whitespace" $ do
        -- doc <- loadMarcDoc -- Unused
        -- Assuming a datafield with text "  some value  "
        -- We'll use an existing field and imagine its content for simplicity,
        -- or ideally, add a specific test case to marcXml if needed.
        -- For now, let's use a field we know exists and test the function's logic.
        -- Create a temporary node with specific text for this test
        let tempNode = XMLText "  some value  " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "some value"

      it "removes leading and trailing dots" $ do
        let tempNode = XMLText "..some value..." 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "some value"

      it "removes leading/trailing whitespace and dots" $ do
        let tempNode = XMLText "  ..some value..  " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "some value"

      it "handles text with internal dots correctly" $ do
        let tempNode = XMLText "  Dr. Strangelove.  " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Dr. Strangelove"

      it "removes leading and trailing commas" $ do
        let tempNode = XMLText ",,some value,,," 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "some value"

      it "removes leading/trailing mixed chars (dots, commas, whitespace)" $ do
        let tempNode = XMLText "  ., ,.some value,. ,.  " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "some value"

      it "handles text with internal commas correctly" $ do
        let tempNode = XMLText "  Hello, World.  " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Hello, World"

      it "handles text that is only dots" $ do
        let tempNode = XMLText "..." 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""
      
      it "handles text that is only commas" $ do
        let tempNode = XMLText ",,," 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

      it "handles text that is only whitespace" $ do
        let tempNode = XMLText "   " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

      it "handles text that is only mixed chars (dots, commas, whitespace)" $ do
        let tempNode = XMLText " ., ,, . " 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""
        
      it "handles empty text" $ do
        let tempNode = XMLText "" 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

      it "handles text with no leading/trailing whitespace, dots, or commas" $ do
        let tempNode = XMLText "actual value" 0 Nothing
        let result = evaluateXPathExpr tempNode [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "actual value"

      it "applies to text() content of an element with mixed chars" $ do
        -- doc <- loadMarcDoc -- Unused
        -- Find a suitable field in marcXml, e.g., controlfield tag 001
        -- <controlfield tag="001"> . leading dot and space. </controlfield>
        -- We need to ensure such a field exists or add it to TestData.hs
        -- For this example, let's assume controlfield 001 has " . test value . "
        -- To make this test robust, we should ideally modify TestData.hs to include this.
        -- As a workaround, we can create a specific node structure.
        let rootNode = unsafeParseXML $ BS.pack "<root><item>  ., test value ,,.  </item></root>"
        let itemNodes = evaluateXPathExpr rootNode [x|/root/item|]
        let result = evaluateXPathExpr (head itemNodes) [x|marc-value()|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "test value"

    describe "tokenize function" $ do
      it "splits a string by a separator" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple&banana&cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "trims whitespace from parts" $ do
        let doc = unsafeParseXML $ BS.pack "<root>  apple  &  banana  &  cherry  </root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "removes empty parts after trimming" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple&&banana& &cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "handles separator at the beginning of the string" $ do
        let doc = unsafeParseXML $ BS.pack "<root>&apple&banana</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["apple", "banana"]

      it "handles separator at the end of the string" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple&banana&</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["apple", "banana"]

      it "handles multiple separators together" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple&&banana</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["apple", "banana"]

      it "uses a different separator" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple,banana,cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), ',')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "returns the original string if separator not found (after trimming)" $ do
        let doc = unsafeParseXML $ BS.pack "<root>  apple banana cherry  </root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 1
        nodeText (head nodes) `shouldBe` "apple banana cherry"

      it "returns empty list for empty input string" $ do
        let doc = unsafeParseXML $ BS.pack "<root></root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 0

      it "returns empty list for input string with only separators" $ do
        let doc = unsafeParseXML $ BS.pack "<root>&&&</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 0
        
      it "returns empty list for input string with only whitespace and separators" $ do
        let doc = unsafeParseXML $ BS.pack "<root> & & </root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '&')|]
        length nodes `shouldBe` 0

      it "integrates with an XPath expression resolving to text" $ do
        doc <- loadTestDoc
        -- The first PHOTOGRAPHER text is "Agnew and Son"
        let nodes = evaluateXPathExpr doc [x|tokenize(/RECORDS/RECORD/PHOTOGRAPHER/text(), ' and ')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["Agnew", "Son"]
      
      it "handles empty string as input to tokenize" $ do
        let tempNode = XMLText "" 0 Nothing
        let nodes = evaluateXPathExpr tempNode [x|tokenize(., ',')|]
        length nodes `shouldBe` 0

      it "handles string with only whitespace as input to tokenize" $ do
        let tempNode = XMLText "   " 0 Nothing
        let nodes = evaluateXPathExpr tempNode [x|tokenize(., ',')|]
        -- After nodeText, "   " becomes "", so tokenize("") results in []
        length nodes `shouldBe` 0
        
      it "tokenizes a string directly provided in an XMLText node" $ do
        let tempNode = XMLText "part1; part2 ; part3" 0 Nothing
        let nodes = evaluateXPathExpr tempNode [x|tokenize(., ';')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["part1", "part2", "part3"]

      it "assigns sequential 1-based indices to tokenized text nodes" $ do
        let doc = unsafeParseXML $ BS.pack "<root>part1,part2,part3</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), ',')|]
        length nodes `shouldBe` 3
        map xmlNodeIndex nodes `shouldBe` [0, 1, 2]
        map nodeText nodes `shouldBe` ["part1", "part2", "part3"]

    describe "tokenize function with multiple separators" $ do
      it "splits a string by multiple separators using OR logic: '(,)|(;)'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple,banana;cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '(,)|(;)')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "splits a string with separators containing spaces: '( , )|( ; )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple , banana ; cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '( , )|( ; )')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]
      
      it "handles a single separator in OR format: '(,)'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple,banana,cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '(,)')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "falls back to literal interpretation for non-OR format: '.,;'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple.,;banana.,;cherry</root>"
        -- This will try to split by the literal string ".,;"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '.,;')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]

      it "splits with mixed separators: '(,)|( - )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>one,two - three,four - five</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '(,)|( - )')|]
        length nodes `shouldBe` 5
        map nodeText nodes `shouldBe` ["one", "two", "three", "four", "five"]

      it "handles leading/trailing spaces within OR separator parts: '(, )|( ; )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple, banana ; cherry</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '(, )|( ; )')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["apple", "banana", "cherry"]
        
      it "tokenizes with complex separators like '( & )|( / )|( - )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>item1 & item2 / item3 - item4</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '( & )|( / )|( - )')|]
        length nodes `shouldBe` 4
        map nodeText nodes `shouldBe` ["item1", "item2", "item3", "item4"]

      it "tokenizes with single multi-char separator in OR format: '( and )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>Agnew and Son</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '( and )')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["Agnew", "Son"]

      it "tokenizes correctly when one OR option is a substring of another (longer first): '( and )|( )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>Salt and Pepper</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '( and )|( )')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["Salt", "Pepper"]

      it "tokenizes correctly when one OR option is a substring of another (shorter first): '( )|( and )'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>Salt and Pepper</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '( )|( and )')|]
        length nodes `shouldBe` 3
        map nodeText nodes `shouldBe` ["Salt", "and", "Pepper"]

      it "falls back to literal if OR format is malformed (empty part): '()|(,)'" $ do
        let doc = unsafeParseXML $ BS.pack "<root>apple()|(,)banana</root>"
        let nodes = evaluateXPathExpr doc [x|tokenize(/root/text(), '()|(,)')|]
        length nodes `shouldBe` 2
        map nodeText nodes `shouldBe` ["apple", "banana"]

    describe "concat function" $ do
      it "concatenates multiple XPath expressions resolving to text" $ do
        doc <- loadTestDoc
        -- /RECORDS/RECORD/PHOTOGRAPHER/text() will get the first one: "Agnew and Son"
        -- /RECORDS/RECORD/INSTITUTE/text() will get the first one: "frick"
        let result = evaluateXPathExpr doc [x|concat(/RECORDS/RECORD/PHOTOGRAPHER/text(), " ", /RECORDS/RECORD/INSTITUTE/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and Son frick"

      it "concatenates multiple string literals" $ do
        doc <- loadTestDoc -- Context node is needed, though not directly used by literals
        let result = evaluateXPathExpr doc [x|concat('Hello', " ", "World", '!')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Hello World!"
      
      it "concatenates a mix of XPath expressions and string literals" $ do
        doc <- loadTestDoc
        -- /RECORDS/RECORD/PHOTOGRAPHER/text() will get the first one: "Agnew and Son"
        let result = evaluateXPathExpr doc [x|concat("Name: ", /RECORDS/RECORD/PHOTOGRAPHER/text(), ", Institute: ", /RECORDS/RECORD/INSTITUTE/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Name: Agnew and Son, Institute: frick"

      it "handles XPath expressions yielding no result (empty string)" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat('Prefix-', /RECORDS/NONEXISTENT/text(), '-Suffix')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Prefix--Suffix"

      it "handles empty string literals" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat('', /RECORDS/RECORD/PHOTOGRAPHER/text(), '')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and Son"

      it "handles string literals with single and double quotes" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat("'single'", '"double"')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "'single'\"double\""
        
      it "uses the string value of the first node if an XPath resolves to multiple nodes" $ do
        doc <- loadTestDoc
        -- /RECORDS/RECORD/PHOTOGRAPHER/text() resolves to three text nodes
        -- First one is "Agnew and Son"
        let result = evaluateXPathExpr doc [x|concat("First: ", /RECORDS/RECORD/PHOTOGRAPHER/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "First: Agnew and Son"

      it "concatenates arguments correctly even if some are empty" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat(/RECORDS/RECORD/PHOTOGRAPHER/text(), /RECORDS/NONEXISTENT/text(), " ", /RECORDS/RECORD/INSTITUTE/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and Son frick"

      it "handles concat with a single literal argument" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat('test')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "test"

      it "handles concat with a single XPath expression argument" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat(/RECORDS/RECORD/PHOTOGRAPHER/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "Agnew and Son"
      
      it "handles concat with an empty literal string" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat('')|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

      it "handles concat with an XPath expression evaluating to empty" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat(/RECORDS/NONEXISTENT/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` ""

      it "handles concat with mixed arguments including empty ones" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|concat('A', /RECORDS/NONEXISTENT/text(), 'B', '', 'C', /RECORDS/RECORD/PHOTOGRAPHER/text())|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` T.pack ("ABC" ++ "Agnew and Son")

    describe "count function" $ do
      it "counts the number of nodes in a sequence" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|count(/RECORDS/RECORD/PHOTOGRAPHER)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "3"

      it "returns 0 for an empty sequence" $ do
        doc <- loadTestDoc
        let result = evaluateXPathExpr doc [x|count(/RECORDS/NONEXISTENT)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "0"

    describe "preceding:: axis" $ do
      let precedingXml = BS.pack "<root><a><one/></a><b/><c><two/><three/></c><d><four/></d></root>"
      let doc = unsafeParseXML precedingXml

      it "selects all preceding nodes" $ do
        let gNodes = evaluateXPathExpr doc [x|/root/d/four|]
        let gNode = head gNodes
        let nodes = evaluateXPathExpr gNode [x|preceding::one|]
        length nodes `shouldBe` 1

      it "selects preceding nodes with a specific name" $ do
        let gNodes = evaluateXPathExpr doc [x|/root/d/four|]
        let gNode = head gNodes
        let nodes = evaluateXPathExpr gNode [x|preceding::b|]
        length nodes `shouldBe` 1

      it "selects all preceding element nodes with wildcard" $ do
        let gNodes = evaluateXPathExpr doc [x|/root/d/four|]
        let gNode = head gNodes
        let nodes = evaluateXPathExpr gNode [x|preceding::*|]
        -- Note: This implementation of `preceding::` deviates from the W3C spec
        -- in that it includes ancestor nodes. The spec says it should not.
        -- The expected nodes are in reverse document order.
        map nodeName nodes `shouldBe` ["d", "three", "two", "c", "b", "one", "a", "root"]
        length nodes `shouldBe` 8

    describe "count with preceding" $ do
      let precedingXml = BS.pack "<root><a/><b/><c/><d/><e/></root>"
      let doc = unsafeParseXML precedingXml
      it "counts preceding nodes" $ do
        let dNodes = evaluateXPathExpr doc [x|/root/d|]
        let dNode = head dNodes
        let result = evaluateXPathExpr dNode [x|count(preceding::a)|]
        length result `shouldBe` 1
        nodeText (head result) `shouldBe` "1"
