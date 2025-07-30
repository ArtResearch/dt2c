module Pharos.DSL.GeneratorSpec (spec) where

import Test.Hspec
import Generators
import DSL (evaluateCondition, equals, exists, contains, starts, and_, or_, not_, equalsXPath, startsWithXPath, containsXPath, isFirst)
import Prelude
import RDF (Value(..))
import qualified Text.XML.Hexml as X
import qualified Data.ByteString as BS
import XML (XMLNode(..), x)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

parseXML :: String -> XMLNode
parseXML xmlString = case X.parse (TE.encodeUtf8 (T.pack xmlString)) of
  Right doc -> XMLNode doc 0 Nothing
  Left err -> error $ "Failed to parse XML: " ++ show err

-- Create an empty context for testing
emptyContext :: GenerationContext
emptyContext = GenerationContext {
  contextNode = TE.encodeUtf8 ".",
  nodeIndex = 0,
  domainUri = "",
  namedUris = Map.empty
}

spec :: Spec
spec = do
  describe "Generator" $ do
    describe "Condition evaluation" $ do
      describe "equals condition" $ do
        it "evaluates true if a single node matches" $ do
            let node = parseXML "<test><value>abc</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "abc") [node]) `shouldBe` 1
        it "evaluates true for case-insensitive match" $ do
            let node = parseXML "<test><value>ABC</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "abc") [node]) `shouldBe` 1
        it "evaluates true for case-insensitive match with mixed case in value" $ do
            let node = parseXML "<test><value>AbC</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "aBc") [node]) `shouldBe` 1
        it "evaluates false if a single node does not match" $ do
            let node = parseXML "<test><value>abc</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "xyz") [node]) `shouldBe` 0
        it "evaluates true if one of multiple nodes matches" $ do
            let node = parseXML "<test><value>xyz</value><value>abc</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "abc") [node]) `shouldBe` 1
        it "evaluates true if one of multiple nodes matches case-insensitively" $ do
            let node = parseXML "<test><value>XYZ</value><value>ABC</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "abc") [node]) `shouldBe` 1
        it "evaluates false if none of multiple nodes match" $ do
            let node = parseXML "<test><value>xyz</value><value>def</value></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "abc") [node]) `shouldBe` 0
        it "evaluates false if xpath finds no nodes" $ do
            let node = parseXML "<test></test>"
            length (evaluateCondition (equals [x|/test/value/text()|] "abc") [node]) `shouldBe` 0

      describe "exists condition" $ do
        it "evaluates true if node exists" $ do
            let node = parseXML "<test><value>abc</value></test>"
            length (evaluateCondition (exists [x|/test/value|]) [node]) `shouldBe` 1
        it "evaluates false if node does not exist" $ do
            let node = parseXML "<test><value>abc</value></test>"
            length (evaluateCondition (exists [x|/test/missing|]) [node]) `shouldBe` 0

      describe "contains condition" $ do
        it "evaluates true if a single node contains the substring" $ do
            let node = parseXML "<test><item>apple</item></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "ppl") [node]) `shouldBe` 1
        it "evaluates true if a single node contains the substring case-insensitively" $ do
            let node = parseXML "<test><item>Apple</item></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "pPl") [node]) `shouldBe` 1
        it "evaluates false if a single node does not contain the substring" $ do
            let node = parseXML "<test><item>apple</item></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "xyz") [node]) `shouldBe` 0
        it "evaluates true if one of multiple nodes contains the substring" $ do
            let node = parseXML "<test><item>apple</item><item>banana</item></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "nan") [node]) `shouldBe` 1
        it "evaluates true if one of multiple nodes contains the substring case-insensitively" $ do
            let node = parseXML "<test><item>Apple</item><item>BaNaNa</item></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "nAn") [node]) `shouldBe` 1
        it "evaluates false if none of multiple nodes contain the substring" $ do
            let node = parseXML "<test><item>apple</item><item>banana</item></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "orange") [node]) `shouldBe` 0
        it "evaluates false if xpath finds no nodes" $ do
            let node = parseXML "<test></test>"
            length (evaluateCondition (contains [x|/test/item/text()|] "any") [node]) `shouldBe` 0

      describe "starts with condition" $ do
        it "evaluates true if a single node starts with the prefix" $ do
            let node = parseXML "<test><name>John Doe</name></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "John") [node]) `shouldBe` 1
        it "evaluates true if a single node starts with the prefix case-insensitively" $ do
            let node = parseXML "<test><name>JOHN Doe</name></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "john") [node]) `shouldBe` 1
        it "evaluates false if a single node does not start with the prefix" $ do
            let node = parseXML "<test><name>John Doe</name></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "Jane") [node]) `shouldBe` 0
        it "evaluates true if one of multiple nodes starts with the prefix" $ do
            let node = parseXML "<test><name>John Doe</name><name>Jane Doe</name></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "Jane") [node]) `shouldBe` 1
        it "evaluates true if one of multiple nodes starts with the prefix case-insensitively" $ do
            let node = parseXML "<test><name>JOHN Doe</name><name>JANE Doe</name></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "jane") [node]) `shouldBe` 1
        it "evaluates false if none of multiple nodes start with the prefix" $ do
            let node = parseXML "<test><name>John Doe</name><name>Jane Doe</name></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "Peter") [node]) `shouldBe` 0
        it "evaluates false if xpath finds no nodes" $ do
            let node = parseXML "<test></test>"
            length (evaluateCondition (starts [x|/test/name/text()|] "any") [node]) `shouldBe` 0

      describe "and condition" $ do
        it "evaluates true if all sub-conditions are true" $ do
          let node = parseXML "<test><a>1</a><b>2</b></test>"
          length (evaluateCondition (and_ [
              exists [x|/test/a|],
              equals [x|/test/b/text()|] "2"
            ]) [node]) `shouldBe` 1
        it "evaluates false if any sub-condition is false" $ do
          let node = parseXML "<test><a>1</a><b>3</b></test>"
          length (evaluateCondition (and_ [
              exists [x|/test/a|],
              equals [x|/test/b/text()|] "2"
            ]) [node]) `shouldBe` 0

      describe "or condition" $ do
        it "evaluates true if any sub-condition is true" $ do
          let node = parseXML "<test><a>1</a></test>"
          length (evaluateCondition (or_ [
              exists [x|/test/missing|],
              exists [x|/test/a|]
            ]) [node]) `shouldBe` 1
        it "evaluates false if all sub-conditions are false" $ do
          let node = parseXML "<test><b>1</b></test>"
          length (evaluateCondition (or_ [
              exists [x|/test/missing|],
              exists [x|/test/another_missing|]
            ]) [node]) `shouldBe` 0

      describe "not condition" $ do
        it "negates a true condition to false" $ do
          let node = parseXML "<test><value>abc</value></test>"
          length (evaluateCondition (not_ (equals [x|/test/value/text()|] "abc")) [node]) `shouldBe` 0
        it "negates a false condition to true" $ do
          let node = parseXML "<test><value>abc</value></test>"
          length (evaluateCondition (not_ (equals [x|/test/value/text()|] "xyz")) [node]) `shouldBe` 1

      describe "equalsXPath condition" $ do
        it "evaluates true if text of nodes from two xpaths are equal" $ do
            let node = parseXML "<doc><a>text1</a><b>text1</b></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 1
        it "evaluates true if text of nodes from two xpaths are equal case-insensitively" $ do
            let node = parseXML "<doc><a>TEXT1</a><b>text1</b></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 1
        it "evaluates false if text of nodes from two xpaths are different" $ do
            let node = parseXML "<doc><a>text1</a><b>text2</b></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 0
        it "evaluates true if one of multiple nodes from first xpath matches one from second" $ do
            let node = parseXML "<doc><a_coll><item>val1</item><item>val2</item></a_coll><b_coll><item>val3</item><item>val2</item></b_coll></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 1
        it "evaluates true if one of multiple nodes from first xpath matches one from second case-insensitively" $ do
            let node = parseXML "<doc><a_coll><item>VAL1</item><item>VAL2</item></a_coll><b_coll><item>val3</item><item>val2</item></b_coll></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 1
        it "evaluates false if no nodes match between two xpaths" $ do
            let node = parseXML "<doc><a_coll><item>val1</item><item>valX</item></a_coll><b_coll><item>val3</item><item>valY</item></b_coll></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 0
        it "evaluates false if first xpath finds no nodes" $ do
            let node = parseXML "<doc><b>text1</b></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/nonexistent/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 0
        it "evaluates false if second xpath finds no nodes" $ do
            let node = parseXML "<doc><a>text1</a></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/a/text()|] [x|/doc/nonexistent/text()|]) [node]) `shouldBe` 0
        it "evaluates false if both xpaths find no nodes" $ do
            let node = parseXML "<doc></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/nonexistent1/text()|] [x|/doc/nonexistent2/text()|]) [node]) `shouldBe` 0
        it "evaluates true for attribute comparison if values match" $ do
            let node = parseXML "<doc><el1 id='123'/><el2 ref='123'/></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 1
        it "evaluates true for attribute comparison if values match case-insensitively" $ do
            let node = parseXML "<doc><el1 id='ABC'/><el2 ref='abc'/></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 1
        it "evaluates false for attribute comparison if values do not match" $ do
            let node = parseXML "<doc><el1 id='123'/><el2 ref='456'/></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 0
        it "evaluates true when comparing an element's text to an attribute's value if they match" $ do
            let node = parseXML "<doc><el1 id='abc'>abc</el1></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/el1/text()|] [x|/doc/el1/@id|]) [node]) `shouldBe` 1
        it "evaluates true when comparing an element's text to an attribute's value if they match case-insensitively" $ do
            let node = parseXML "<doc><el1 id='ABC'>abc</el1></doc>"
            length (evaluateCondition (equalsXPath [x|/doc/el1/text()|] [x|/doc/el1/@id|]) [node]) `shouldBe` 1

      describe "startsWithXPath condition" $ do
        it "evaluates true if text from first xpath starts with text from second xpath" $ do
            let node = parseXML "<doc><a>HelloWorld</a><b>Hello</b></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 1
        it "evaluates true if text from first xpath starts with text from second xpath case-insensitively" $ do
            let node = parseXML "<doc><a>HELLOWORLD</a><b>hello</b></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 1
        it "evaluates false if text from first xpath does not start with text from second xpath" $ do
            let node = parseXML "<doc><a>WorldHello</a><b>Hello</b></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 0
        it "evaluates true if one of multiple texts from first xpath starts with one from second" $ do
            let node = parseXML "<doc><a_coll><item>GoodbyeWorld</item><item>HelloWorld</item></a_coll><b_coll><item>Hi</item><item>Hello</item></b_coll></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 1
        it "evaluates true if one of multiple texts from first xpath starts with one from second case-insensitively" $ do
            let node = parseXML "<doc><a_coll><item>GOODBYEWORLD</item><item>HELLOWORLD</item></a_coll><b_coll><item>hi</item><item>hello</item></b_coll></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 1
        it "evaluates false if no text from first xpath starts with any text from second" $ do
            let node = parseXML "<doc><a_coll><item>Goodbye</item><item>Farewell</item></a_coll><b_coll><item>Hello</item><item>Hi</item></b_coll></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 0
        it "evaluates false if first xpath finds no nodes" $ do
            let node = parseXML "<doc><b>Hello</b></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/nonexistent/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 0
        it "evaluates false if second xpath finds no nodes" $ do
            let node = parseXML "<doc><a>HelloWorld</a></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/a/text()|] [x|/doc/nonexistent/text()|]) [node]) `shouldBe` 0
        it "evaluates true for attribute comparison if first starts with second" $ do
            let node = parseXML "<doc><el1 id='HelloCruelWorld'/><el2 ref='Hello'/></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 1
        it "evaluates true for attribute comparison if first starts with second case-insensitively" $ do
            let node = parseXML "<doc><el1 id='HELLOCRUELWORLD'/><el2 ref='hello'/></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 1
        it "evaluates true when comparing element text starting with an attribute value" $ do
            let node = parseXML "<doc><el1 id='prefix'>prefixAndSuffix</el1></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/el1/text()|] [x|/doc/el1/@id|]) [node]) `shouldBe` 1
        it "evaluates true when comparing element text starting with an attribute value case-insensitively" $ do
            let node = parseXML "<doc><el1 id='PREFIX'>prefixAndSuffix</el1></doc>"
            length (evaluateCondition (startsWithXPath [x|/doc/el1/text()|] [x|/doc/el1/@id|]) [node]) `shouldBe` 1

      describe "containsXPath condition" $ do
        it "evaluates true if text from first xpath contains text from second xpath" $ do
            let node = parseXML "<doc><a>HelloWorld</a><b>oWo</b></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 1
        it "evaluates true if text from first xpath contains text from second xpath case-insensitively" $ do
            let node = parseXML "<doc><a>HELLOWORLD</a><b>owo</b></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 1
        it "evaluates false if text from first xpath does not contain text from second xpath" $ do
            let node = parseXML "<doc><a>HelloWorld</a><b>XYZ</b></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 0
        it "evaluates true if one of multiple texts from first xpath contains one from second" $ do
            let node = parseXML "<doc><a_coll><item>abc</item><item>def</item></a_coll><b_coll><item>xyz</item><item>e</item></b_coll></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 1
        it "evaluates true if one of multiple texts from first xpath contains one from second case-insensitively" $ do
            let node = parseXML "<doc><a_coll><item>ABC</item><item>DEF</item></a_coll><b_coll><item>xyz</item><item>E</item></b_coll></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 1
        it "evaluates false if no text from first xpath contains any text from second" $ do
            let node = parseXML "<doc><a_coll><item>abc</item><item>def</item></a_coll><b_coll><item>xyz</item><item>ghi</item></b_coll></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a_coll/item/text()|] [x|/doc/b_coll/item/text()|]) [node]) `shouldBe` 0
        it "evaluates false if first xpath finds no nodes" $ do
            let node = parseXML "<doc><b>oWo</b></doc>"
            length (evaluateCondition (containsXPath [x|/doc/nonexistent/text()|] [x|/doc/b/text()|]) [node]) `shouldBe` 0
        it "evaluates false if second xpath finds no nodes" $ do
            let node = parseXML "<doc><a>HelloWorld</a></doc>"
            length (evaluateCondition (containsXPath [x|/doc/a/text()|] [x|/doc/nonexistent/text()|]) [node]) `shouldBe` 0
        it "evaluates true for attribute comparison if first contains second" $ do
            let node = parseXML "<doc><el1 id='BigLongString'/><el2 ref='Long'/></doc>"
            length (evaluateCondition (containsXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 1
        it "evaluates true for attribute comparison if first contains second case-insensitively" $ do
            let node = parseXML "<doc><el1 id='BIGLONGSTRING'/><el2 ref='long'/></doc>"
            length (evaluateCondition (containsXPath [x|/doc/el1/@id|] [x|/doc/el2/@ref|]) [node]) `shouldBe` 1
        it "evaluates true when comparing element text containing an attribute value" $ do
            let node = parseXML "<doc><el1 id='Substring'>ThisIsASubstringExample</el1></doc>"
            length (evaluateCondition (containsXPath [x|/doc/el1/text()|] [x|/doc/el1/@id|]) [node]) `shouldBe` 1
        it "evaluates true when comparing element text containing an attribute value case-insensitively" $ do
            let node = parseXML "<doc><el1 id='SUBSTRING'>ThisIsASubstringExample</el1></doc>"
            length (evaluateCondition (containsXPath [x|/doc/el1/text()|] [x|/doc/el1/@id|]) [node]) `shouldBe` 1

      describe "isFirst condition" $ do
        it "returns only the first node from a list of multiple nodes" $ do
          let nodes = [parseXML "<item>1</item>", parseXML "<item>2</item>", parseXML "<item>3</item>"]
          let result = evaluateCondition isFirst nodes
          length result `shouldBe` 1
        it "returns the single node if list contains only one" $ do
          let nodes = [parseXML "<item>single</item>"]
          let result = evaluateCondition isFirst nodes
          length result `shouldBe` 1
        it "returns an empty list if input is empty" $ do
          let nodes = [] :: [XMLNode]
          length (evaluateCondition isFirst nodes) `shouldBe` 0

    describe "LiteralGen" $ do
      it "creates a literal from a constant value" $ do
        let (_, gen) = literal ("constant value" :: T.Text)
        (result, _) <- generateId gen undefined undefined undefined emptyContext
        result `shouldBe` Literal "constant value" Nothing

      it "creates a literal from an xpath expression" $ do
        let node = parseXML "<test><value>xpath value</value></test>"
        let (_, gen) = literal [x|/test/value/text()|]
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "xpath value" Nothing

      it "creates a typed literal from an xpath expression" $ do
        let node = parseXML "<test><value>xpath value</value></test>"
        let intType = PrefixedURI (T.pack "xsd:integer")
        let (_, gen) = typedLiteral [x|/test/value/text()|] intType
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "xpath value" (Just intType)

      it "creates a literal from the node index" $ do
        let node = (parseXML "<test/>") { xmlNodeIndex = 5 }
        let (_, gen) = literal i
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "6" Nothing

      it "creates a typed literal from the node index" $ do
        let node = (parseXML "<test/>") { xmlNodeIndex = 9 }
        let stringType = PrefixedURI (T.pack "xsd:string")
        let (_, gen) = typedLiteral i stringType
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "10" (Just stringType)

    describe "DateTimeGen" $ do
      let xsdDateTime = Just (PrefixedURI (T.pack "xsd:dateTime"))
      describe "Year type" $ do
        it "requires upper bound" $ do
          let node = parseXML "<test><date>2024</date></test>"
          let gen = DateTimeValueGen [x|/test/date/text()|] (Year, Upper)
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "2024-12-31T23:59:59Z" xsdDateTime

        it "requires lower bound" $ do
          let node = parseXML "<test><date>2024</date></test>"
          let gen = DateTimeValueGen [x|/test/date/text()|] (Year, Lower)
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "2024-01-01T00:00:00Z" xsdDateTime

      describe "FullDate type" $ do
        it "requires upper bound" $ do
          let node = parseXML "<test><date>2024-06-15</date></test>"
          let gen = DateTimeValueGen [x|/test/date/text()|] (FullDate, Upper)
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "2024-06-15T23:59:59Z" xsdDateTime

        it "requires lower bound" $ do
          let node = parseXML "<test><date>2024-06-15</date></test>"
          let gen = DateTimeValueGen [x|/test/date/text()|] (FullDate, Lower)
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "2024-06-15T00:00:00Z" xsdDateTime

      describe "DateTime type" $ do
        it "accepts datetime without bounds" $ do
          let node = parseXML "<test><date>2024-06-15T14:30:00Z</date></test>"
          let gen = DateTimeValueGen [x|/test/date/text()|] DateTime
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "2024-06-15T14:30:00Z" xsdDateTime

      describe "Edge cases" $ do
        it "handles empty xpath result" $ do
          let node = parseXML "<test></test>"
          let gen = DateTimeValueGen [x|/test/nonexistent/text()|] (Year, Upper)
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "unknown" xsdDateTime

        it "handles multiple xpath results (takes first)" $ do
          let node = parseXML "<test><date>2024</date><date>2025</date></test>"
          let gen = DateTimeValueGen [x|/test/date/text()|] (Year, Upper)
          (result, _) <- generateId gen node undefined undefined emptyContext
          result `shouldBe` Literal "2024-12-31T23:59:59Z" xsdDateTime

    describe "TemplateGen" $ do
        it "replaces template variables correctly" $ do
            let template = "work/{objId}/photo/{photoId}/feature/{side}/visual_item/image/id"
            let values = [("objId", "123" :: BS.ByteString), ("photoId", "456"), ("side", "front")]
            let gen = TemplateGen template values
            (result, _) <- generateId gen undefined undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "work/123/photo/456/feature/front/visual_item/image/id"

        it "leaves unmatched variables unchanged" $ do
            let template = "work/{objId}/photo/{unknown}"
            let values = [("objId", "123" :: BS.ByteString)]
            let gen = TemplateGen template values
            (result, _) <- generateId gen undefined undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "work/123/photo/{unknown}"

    describe "TemplateGen with XPath" $ do
        it "evaluates simple xpath" $ do
            let node = parseXML "<test>hello</test>"
            let gen = TemplateGen "{val}" [("val", [x|/test|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "hello"

        it "combines template with xpath result" $ do
            let node = parseXML "<test><value>abc</value></test>"
            let gen = TemplateGen "prefix/{val}" [("val", [x|/test/value/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "prefix/abc"

        it "handles empty xpath result" $ do
            let node = parseXML "<test></test>"
            let gen = TemplateGen "prefix/{val}" [("val", [x|nonexistent/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "prefix/"

        it "uses first xpath result when multiple exist" $ do
            let node = parseXML "<test><value>first</value><value>second</value></test>"
            let gen = TemplateGen "prefix/{val}" [("val", [x|/test/value/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "prefix/first"

        it "handles complex template paths" $ do
            let node = parseXML "<test><type>photo</type></test>"
            let gen = TemplateGen "https://artresearch.net/resource/pharos/vocabulary/meta/{type}" [("type", [x|/test/type/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "https://artresearch.net/resource/pharos/vocabulary/meta/photo"

        it "makes only XPath values URL-friendly while preserving template structure" $ do
            let node = parseXML "<test><value>Kuenstler &amp; Cie.</value></test>"
            let gen = TemplateGen "prefix/Special&Path/{val}/Unchanged" [("val", [x|/test/value/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "prefix/Special&Path/kuenstler_and_cie/Unchanged"

        it "normalizes multiple underscores and special characters in XPath values only" $ do
            let node = parseXML "<test><value>Test,Value/With.Multiple___Separators</value></test>"
            let gen = TemplateGen "prefix/Multiple___Path/{val}/Still___Here" [("val", [x|/test/value/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "prefix/Multiple___Path/test_value_with_multiple_separators/Still___Here"

        it "handles diacritics in XPath values while preserving constant parts" $ do
            let node = parseXML "<test><value>éàçñ áèìòù</value></test>"
            let gen = TemplateGen "prefix/{val}/suffix" [("val", [x|/test/value/text()|])]
            (result, _) <- generateId gen node undefined undefined emptyContext
            result `shouldBe` NonPrefixedURI "prefix/e_a_c_n_a_e_i_o_u/suffix"

    describe "RelativeUriTGen" $ do
      it "evaluates template and combines with domain" $ do
        let node = parseXML "<test><value>abc</value></test>"
        let gen = RelativeUriTGen "/path/{val}" [("val", [x|/test/value/text()|])]
        (result, _) <- generateId gen node "https://example.com" undefined emptyContext
        result `shouldBe` NonPrefixedURI "https://example.com/path/abc"

      it "handles empty domain" $ do
        let node = parseXML "<test><value>abc</value></test>"
        let gen = RelativeUriTGen "/path/{val}" [("val", [x|/test/value/text()|])]
        (result, _) <- generateId gen node "" undefined emptyContext
        result `shouldBe` NonPrefixedURI "/path/abc"

      it "handles complex template with multiple variables" $ do
        let node = parseXML "<test><id>123</id><type>photo</type></test>"
        let gen = RelativeUriTGen "/work/{id}/feature/{type}" [
                ("id", [x|/test/id/text()|]),
                ("type", [x|/test/type/text()|])
              ]
        (result, _) <- generateId gen node "https://example.com" undefined emptyContext
        result `shouldBe` NonPrefixedURI "https://example.com/work/123/feature/photo"

      it "handles empty xpath result" $ do
        let node = parseXML "<test></test>"
        let gen = RelativeUriTGen "/path/{val}" [("val", [x|nonexistent/text()|])]
        (result, _) <- generateId gen node "https://example.com" undefined emptyContext
        result `shouldBe` NonPrefixedURI "https://example.com/path/"

      it "makes only XPath values URL-friendly in relative URIs" $ do
        let node = parseXML "<test><value>Kuenstler &amp; Cie.</value></test>"
        let gen = RelativeUriTGen "/Special&Path/{val}/Unchanged" [("val", [x|/test/value/text()|])]
        (result, _) <- generateId gen node "https://example.com" undefined emptyContext
        result `shouldBe` NonPrefixedURI "https://example.com/Special&Path/kuenstler_and_cie/Unchanged"

      it "normalizes XPath values while preserving template structure in relative URIs" $ do
        let node = parseXML "<test><value>Test,Value/With.Multiple___Separators</value></test>"
        let gen = RelativeUriTGen "/Multiple___Path/{val}/Still___Here" [("val", [x|/test/value/text()|])]
        (result, _) <- generateId gen node "https://example.com" undefined emptyContext
        result `shouldBe` NonPrefixedURI "https://example.com/Multiple___Path/test_value_with_multiple_separators/Still___Here"

      it "handles diacritics in XPath values while preserving template in relative URIs" $ do
        let node = parseXML "<test><value>éàçñ áèìòù</value></test>"
        let gen = RelativeUriTGen "/prefix/{val}/suffix" [("val", [x|/test/value/text()|])]
        (result, _) <- generateId gen node "https://example.com" undefined emptyContext
        result `shouldBe` NonPrefixedURI "https://example.com/prefix/e_a_c_n_a_e_i_o_u/suffix"

    describe "NamedGen and NamedUriGen" $ do
      it "stores a generated value with a name" $ do
        let node = parseXML "<test><value>abc</value></test>"
        let gen = named "myUri" (constUri "http://example.org/resource")
        (result, ctx) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` NonPrefixedURI "http://example.org/resource"
        Map.lookup "myUri" (namedUris ctx) `shouldBe` Just (NonPrefixedURI "http://example.org/resource")

      it "retrieves a previously stored named URI" $ do
        let node = parseXML "<test></test>"
        -- First create a named URI
        let gen1 = named "myUri" (constUri "http://example.org/resource")
        (_, ctx) <- generateId gen1 node undefined undefined emptyContext
        -- Then reference it
        let gen2 = namedUri "myUri"
        (result, _) <- generateId gen2 node undefined undefined ctx
        result `shouldBe` NonPrefixedURI "http://example.org/resource"

      it "handles missing named URI reference" $ do
        let node = parseXML "<test></test>"
        let gen = namedUri "nonExistentUri"
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` EmptyURI

      it "can store and retrieve a template-based URI" $ do
        let node = parseXML "<test><id>123</id></test>"
        let templateGen = templateUri "resource/{id}" [("id", [x|/test/id/text()|])]
        let gen = named "templateUri" templateGen
        (_, ctx) <- generateId gen node undefined undefined emptyContext
        let refGen = namedUri "templateUri"
        (result, _) <- generateId refGen node undefined undefined ctx
        result `shouldBe` NonPrefixedURI "resource/123"

      it "can chain multiple named URIs" $ do
        let node = parseXML "<test></test>"
        -- Create first named URI
        let gen1 = named "uri1" (constUri "http://example.org/resource1")
        (_, ctx1) <- generateId gen1 node undefined undefined emptyContext
        -- Create second named URI that references the first
        let gen2 = named "uri2" (namedUri "uri1")
        (_, ctx2) <- generateId gen2 node undefined undefined ctx1
        -- Reference the second URI
        let gen3 = namedUri "uri2"
        (result, _) <- generateId gen3 node undefined undefined ctx2
        result `shouldBe` NonPrefixedURI "http://example.org/resource1"

    describe "LiteralNodeFnGen" $ do
      it "applies a simple function to the node text" $ do
        let node = parseXML "<test><value>  hello world  </value></test>"
        let (_, gen) = literalFn [x|/test/value|] T.strip
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "hello world" Nothing

      it "returns empty literal if xpath finds no node" $ do
        let node = parseXML "<test></test>"
        let (_, gen) = literalFn [x|/test/nonexistent|] T.strip
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "" Nothing

      it "applies a function that uses node structure (e.g., toUpper)" $ do
        let node = parseXML "<test><value>lowercase</value></test>"
        let (_, gen) = literalFn [x|/test/value|] T.toUpper
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "LOWERCASE" Nothing

      it "handles multiple matching nodes (uses first)" $ do
        let node = parseXML "<test><value>first</value><value>second</value></test>"
        let (_, gen) = literalFn [x|/test/value|] T.toUpper
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "FIRST" Nothing

    describe "LiteralMultiNodeFnGen" $ do
      it "concatenates text from two nodes" $ do
        let node = parseXML "<test><first>Hello</first><second>World</second></test>"
        let (_, gen) = literalMultiExprFn [[x|/test/first/text()|], [x|/test/second/text()|]] (T.concat)
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "HelloWorld" Nothing

      it "handles one missing node by passing empty string to function" $ do
        let node = parseXML "<test><first>Hello</first></test>"
        let (_, gen) = literalMultiExprFn [[x|/test/first/text()|], [x|/test/nonexistent/text()|]] (T.intercalate " ")
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "Hello " Nothing -- Note: space from intercalate

      it "handles all missing nodes by passing empty strings to function" $ do
        let node = parseXML "<test></test>"
        let (_, gen) = literalMultiExprFn [[x|/test/nonexistent1/text()|], [x|/test/nonexistent2/text()|]] (T.intercalate "-")
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "-" Nothing -- Note: separator from intercalate

      it "applies a function to join texts with a separator" $ do
        let node = parseXML "<test><part1>A</part1><part2>B</part2><part3>C</part3></test>"
        let (_, gen) = literalMultiExprFn [[x|/test/part1/text()|], [x|/test/part2/text()|], [x|/test/part3/text()|]] (T.intercalate ", ")
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "A, B, C" Nothing

      it "uses first node if multiple match an xpath" $ do
        let node = parseXML "<test><item>1</item><item>2</item><other>A</other></test>"
        let (_, gen) = literalMultiExprFn [[x|/test/item/text()|], [x|/test/other/text()|]] (T.intercalate "")
        (result, _) <- generateId gen node undefined undefined emptyContext
        result `shouldBe` Literal "1A" Nothing
