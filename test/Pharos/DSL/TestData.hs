module Pharos.DSL.TestData (photographersXml, marcXml) where

import Util.QQ (w)

photographersXml = [w|<?xml version="1.0" encoding="UTF-8"?>
<RECORDS>
    <RECORD>
        <URI>https://artresearch.net/resource/frick/actor/agnew_and_son</URI>
        <RECONCILED_URI>http://www.wikidata.org/wiki/Q17021787</RECONCILED_URI>
        <POSSIBLE_UMBRELLA_TERM_URI>https://artresearch.net/resource/pharos/group/agnew</POSSIBLE_UMBRELLA_TERM_URI>
        <PHOTOGRAPHER>Agnew and Son</PHOTOGRAPHER>
        <INSTITUTE>frick</INSTITUTE>
        <WIKIDATA>http://www.wikidata.org/wiki/Q17021787</WIKIDATA>
        <POSSIBLE_UMBRELLA_TERM>Agnew</POSSIBLE_UMBRELLA_TERM>
        <SEPARATE_ENTITY>Agnew and Sons</SEPARATE_ENTITY>
    </RECORD>
    <RECORD>
        <URI>https://artresearch.net/resource/frick/actor/agnew_and_son_london</URI>
        <RECONCILED_URI>http://www.wikidata.org/wiki/Q17021787</RECONCILED_URI>
        <POSSIBLE_UMBRELLA_TERM_URI>https://artresearch.net/resource/pharos/group/agnew</POSSIBLE_UMBRELLA_TERM_URI>
        <PHOTOGRAPHER>Agnew and Son, London</PHOTOGRAPHER>
        <INSTITUTE>frick</INSTITUTE>
        <WIKIDATA>http://www.wikidata.org/wiki/Q17021787</WIKIDATA>
        <POSSIBLE_UMBRELLA_TERM>Agnew</POSSIBLE_UMBRELLA_TERM>
        <SEPARATE_ENTITY>Agnew and Sons</SEPARATE_ENTITY>
    </RECORD>
    <RECORD>
        <URI>https://artresearch.net/resource/marburg/actor/ahlers_henrik</URI>
        <RECONCILED_URI>http://www.wikidata.org/entity/Q126720734</RECONCILED_URI>
        <POSSIBLE_UMBRELLA_TERM_URI>https://artresearch.net/resource/pharos/group/ahlers</POSSIBLE_UMBRELLA_TERM_URI>
        <PHOTOGRAPHER>Ahlers, Henrik</PHOTOGRAPHER>
        <INSTITUTE>marburg</INSTITUTE>
        <WIKIDATA>http://www.wikidata.org/entity/Q126720734</WIKIDATA>
        <POSSIBLE_UMBRELLA_TERM>Ahlers</POSSIBLE_UMBRELLA_TERM>
        <SEPARATE_ENTITY>Ahlers, Henrik</SEPARATE_ENTITY>
        <ENTITY_TYPE>person</ENTITY_TYPE>
    </RECORD>
</RECORDS>|]

marcXml = [w|<?xml version="1.0" encoding="UTF-8"?>
<record>
    <controlfield tag="008">110429k18431843xx nnn |      |   cneng|d</controlfield>
    <datafield tag="245" ind1="1" ind2="0">
        <subfield code="a">The title of the work</subfield>
        <subfield code="b">subtitle</subfield>
    </datafield>
    <datafield tag="100" ind1="1" ind2=" ">
        <subfield code="a">Author, Test</subfield>
        <subfield code="d">1900-1999</subfield>
    </datafield>
    <datafield tag="245" ind1="0" ind2="0">
        <subfield code="a">Another title</subfield>
    </datafield>
    <datafield tag="590" ind1="9" ind2=" ">
        <subfield code="a">Some note</subfield>
    </datafield>
    <datafield tag="590" ind1="0" ind2=" ">
        <subfield code="a">Different note</subfield>
    </datafield>
</record>|]
