# DT2C: Any data to CIDOC CRM Mapping DSL

This library provides a Haskell-based Domain Specific Language (DSL) for mapping XML (CSV, JSON, RDF, comming later) data sources to RDF triples, specifically targeting the CIDOC CRM ontology. It allows defining declarative mappings that specify how XML elements and attributes correspond to CRM entities and properties.

## Examples

*(Snippets adapted from zeri, frick, pmc mappings)*

**1. Basic Identifier Mapping (Zeri)**

```haskell
-- Context: <PARAGRAFO etichetta="CODES"><NRSCHEDA>123</NRSCHEDA></PARAGRAFO>
[x|PARAGRAFO[@etichetta="CODES"]/NRSCHEDA|] @>> [
    -- Map P1 -> E42 Identifier (relative URI) -> P2 (Type), P190 (Value)
    P1 ---> (E42, relativeUri "/id") ==> [
        P2 --> (E55, nrscheda), -- Vocabulary term
        P190 --> literal [x|text()|] -- Literal value from <NRSCHEDA>
    ],
    -- Map P1 -> E42 Identifier (template URI)
    P1 --> (E42, templateUri "work/{id}" [("id", [x|text()|])])
]
```

**2. Conditional Mapping & Complex Generator (Frick Artist)**

```haskell
-- Context: <datafield tag="100"> <subfield code="a">Name</subfield> <subfield code="j">attributed to</subfield> </datafield>
[x|datafield[@tag='100']|] @>
    -- Only map if subfield 'j' doesn't exist OR equals 'attributed to' OR 'contributor'
    when (or_ [
        not_ (exists [x| subfield[@code='j'] |]),
        equals [x|subfield[@code='j']/text()|] "attributed to",
        equals [x|subfield[@code='j']/text()|] "contributor"
    ]) (
        P108i ---> (E12, relativeUri "/production") ==> [ -- P108i -> E12 Production
            P01i ---> (PC14, ...) ==> [ -- P01i -> PC14 Carried Out By
                P02 --> (E39, templateUri "actor/{id}" [ -- P02 -> E39 Actor (URI from concatenated fields)
                    ("id", [x|concat(subfield[@code='a']/text()+subfield[@code='c']/text())|])
                ])
            ]
        ]
    )
```

**3. DateTime & Substring (Frick Production Date)**

```haskell
-- Context: <controlfield tag="008">...k19501955...</controlfield>
[x|controlfield[@tag='008']|] @> (
    P108i ---> (E12, relativeUri "/production") ==> [ -- P108i -> E12 Production
        P4 ---> (E52, relativeUri "/timespan") ==> [ -- P4 -> E52 Timespan
            P2 --> (E55, FrickMeta.approximate_production_date), -- Type
            -- Earliest date from substring
            P82a --> dateTime [x|substring(substring-after(text(), 'k'), 1, 4)|] (Year, Lower),
            -- Latest date from substring
            P82b --> dateTime [x|substring(substring-after(text(), 'k'), 5, 4)|] (Year, Upper)
        ]
    ]
)
```

**4. Literal Function (Frick Catalog URL)**

```haskell
-- Context: <controlfield tag="001">ALMA123</controlfield>
catalogUrl =
  [x| controlfield[@tag='001'] |] @> (
    P1 ---> (E42, relativeUri "/id/catalog_url") ==> [
      P2 --> (E55, controlfield_001_link),
      -- Prepend base URL to the ID from text()
      P190 --> literalFn [x| text() |] (\n -> "https://library.frick.org/.../alma" <> nodeText n)
    ]
  )
```
