{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module CidocCRM where

import Data.Text (Text)

-- | Basic CRM class types based on CIDOC CRM v7.1.3
data Class_
  = E1_    -- E1_CRM_Entity
  | E2_    -- E2_Temporal_Entity
  | E3_    -- E3_Condition_State
  | E4_    -- E4_Period
  | E5_    -- E5_Event
  | E6_    -- E6_Destruction
  | E7_    -- E7_Activity
  | E8_    -- E8_Acquisition
  | E9_    -- E9_Move
  | E10_   -- E10_Transfer_of_Custody
  | E11_   -- E11_Modification
  | E12_   -- E12_Production
  | E13_   -- E13_Attribute_Assignment
  | E14_   -- E14_Condition_Assessment
  | E15_   -- E15_Identifier_Assignment
  | E16_   -- E16_Measurement
  | E17_   -- E17_Type_Assignment
  | E18_   -- E18_Physical_Thing
  | E19_   -- E19_Physical_Object
  | E20_   -- E20_Biological_Object
  | E21_   -- E21_Person
  | E22_   -- E22_Human-Made_Object
  | E24_   -- E24_Physical_Human-Made_Thing
  | E25_   -- E25_Human-Made_Feature
  | E26_   -- E26_Physical_Feature
  | E27_   -- E27_Site
  | E28_   -- E28_Conceptual_Object
  | E29_   -- E29_Design_or_Procedure
  | E30_   -- E30_Right
  | E31_   -- E31_Document
  | E32_   -- E32_Authority_Document
  | E33_   -- E33_Linguistic_Object
  | E33_E41_ -- E33_E41_Linguistic_Appellation (Custom class from RDF)
  | E34_   -- E34_Inscription
  | E35_   -- E35_Title
  | E36_   -- E36_Visual_Item
  | E37_   -- E37_Mark
  | E39_   -- E39_Actor
  | E41_   -- E41_Appellation
  | E42_   -- E42_Identifier
  | E52_   -- E52_Time-Span
  | E53_   -- E53_Place
  | E54_   -- E54_Dimension
  | E55_   -- E55_Type
  | E56_   -- E56_Language
  | E57_   -- E57_Material
  | E58_   -- E58_Measurement_Unit
  | E63_   -- E63_Beginning_of_Existence
  | E64_   -- E64_End_of_Existence
  | E65_   -- E65_Creation
  | E66_   -- E66_Formation
  | E67_   -- E67_Birth
  | E68_   -- E68_Dissolution
  | E69_   -- E69_Death
  | E70_   -- E70_Thing
  | E71_   -- E71_Human-Made_Thing
  | E72_   -- E72_Legal_Object
  | E73_   -- E73_Information_Object
  | E74_   -- E74_Group
  | E77_   -- E77_Persistent_Item
  | E78_   -- E78_Curated_Holding
  | E79_   -- E79_Part_Addition
  | E80_   -- E80_Part_Removal
  | E81_   -- E81_Transformation
  | E83_   -- E83_Type_Creation
  | E85_   -- E85_Joining
  | E86_   -- E86_Leaving
  | E87_   -- E87_Curation_Activity
  | E89_   -- E89_Propositional_Object
  | E90_   -- E90_Symbolic_Object
  | E92_   -- E92_Spacetime_Volume
  | E93_   -- E93_Presence
  | E96_   -- E96_Purchase
  | E97_   -- E97_Monetary_Amount
  | E98_   -- E98_Currency
  | E99_   -- E99_Product_Type
  | PC14_  -- PC14_carried_out_by (Keep existing custom class?) - Assuming yes for now
  | D1_    -- D1_Image
  | Literal_
  deriving (Show, Eq)

-- | Reference to a CRM class
-------------------------------------------------------------------------------
-- Value level representations
-------------------------------------------------------------------------------
newtype ClassRef (c :: Class_) = ClassRef { unClassRef :: String }

-------------------------------------------------------------------------------
-- SUBCLASS RELATIONSHIPS (Explicitly defined including transitive)
-------------------------------------------------------------------------------

type family IsSubClassOf (c1 :: Class_) (c2 :: Class_) :: Bool where
  IsSubClassOf c c = 'True

  -- E1 is top
  IsSubClassOf 'E2_ 'E1_ = 'True
  IsSubClassOf 'E52_ 'E1_ = 'True
  IsSubClassOf 'E53_ 'E1_ = 'True
  IsSubClassOf 'E54_ 'E1_ = 'True
  IsSubClassOf 'E77_ 'E1_ = 'True
  IsSubClassOf 'E92_ 'E1_ = 'True

  -- E2 -> E1
  IsSubClassOf 'E3_ 'E2_ = 'True
  IsSubClassOf 'E3_ 'E1_ = 'True
  IsSubClassOf 'E4_ 'E2_ = 'True
  IsSubClassOf 'E4_ 'E1_ = 'True

  -- E4 -> E2, E92 -> E1
  IsSubClassOf 'E5_ 'E4_ = 'True
  IsSubClassOf 'E5_ 'E2_ = 'True
  IsSubClassOf 'E5_ 'E92_ = 'True
  IsSubClassOf 'E5_ 'E1_ = 'True

  -- E5 -> E4 -> E2 -> E1
  -- E5 -> E4 -> E92 -> E1
  IsSubClassOf 'E7_ 'E5_ = 'True
  IsSubClassOf 'E7_ 'E4_ = 'True
  IsSubClassOf 'E7_ 'E2_ = 'True
  IsSubClassOf 'E7_ 'E92_ = 'True
  IsSubClassOf 'E7_ 'E1_ = 'True
  IsSubClassOf 'E63_ 'E5_ = 'True
  IsSubClassOf 'E63_ 'E4_ = 'True
  IsSubClassOf 'E63_ 'E2_ = 'True
  IsSubClassOf 'E63_ 'E92_ = 'True
  IsSubClassOf 'E63_ 'E1_ = 'True
  IsSubClassOf 'E64_ 'E5_ = 'True
  IsSubClassOf 'E64_ 'E4_ = 'True
  IsSubClassOf 'E64_ 'E2_ = 'True
  IsSubClassOf 'E64_ 'E92_ = 'True
  IsSubClassOf 'E64_ 'E1_ = 'True

  -- E7 -> E5 -> E4 -> E2 -> E1
  -- E7 -> E5 -> E4 -> E92 -> E1
  IsSubClassOf 'E8_ 'E7_ = 'True
  IsSubClassOf 'E8_ 'E5_ = 'True
  IsSubClassOf 'E8_ 'E4_ = 'True
  IsSubClassOf 'E8_ 'E2_ = 'True
  IsSubClassOf 'E8_ 'E92_ = 'True
  IsSubClassOf 'E8_ 'E1_ = 'True
  IsSubClassOf 'E9_ 'E7_ = 'True
  IsSubClassOf 'E9_ 'E5_ = 'True
  IsSubClassOf 'E9_ 'E4_ = 'True
  IsSubClassOf 'E9_ 'E2_ = 'True
  IsSubClassOf 'E9_ 'E92_ = 'True
  IsSubClassOf 'E9_ 'E1_ = 'True
  IsSubClassOf 'E10_ 'E7_ = 'True
  IsSubClassOf 'E10_ 'E5_ = 'True
  IsSubClassOf 'E10_ 'E4_ = 'True
  IsSubClassOf 'E10_ 'E2_ = 'True
  IsSubClassOf 'E10_ 'E92_ = 'True
  IsSubClassOf 'E10_ 'E1_ = 'True
  IsSubClassOf 'E11_ 'E7_ = 'True
  IsSubClassOf 'E11_ 'E5_ = 'True
  IsSubClassOf 'E11_ 'E4_ = 'True
  IsSubClassOf 'E11_ 'E2_ = 'True
  IsSubClassOf 'E11_ 'E92_ = 'True
  IsSubClassOf 'E11_ 'E1_ = 'True
  IsSubClassOf 'E13_ 'E7_ = 'True
  IsSubClassOf 'E13_ 'E5_ = 'True
  IsSubClassOf 'E13_ 'E4_ = 'True
  IsSubClassOf 'E13_ 'E2_ = 'True
  IsSubClassOf 'E13_ 'E92_ = 'True
  IsSubClassOf 'E13_ 'E1_ = 'True
  IsSubClassOf 'E85_ 'E7_ = 'True
  IsSubClassOf 'E85_ 'E5_ = 'True
  IsSubClassOf 'E85_ 'E4_ = 'True
  IsSubClassOf 'E85_ 'E2_ = 'True
  IsSubClassOf 'E85_ 'E92_ = 'True
  IsSubClassOf 'E85_ 'E1_ = 'True
  IsSubClassOf 'E86_ 'E7_ = 'True
  IsSubClassOf 'E86_ 'E5_ = 'True
  IsSubClassOf 'E86_ 'E4_ = 'True
  IsSubClassOf 'E86_ 'E2_ = 'True
  IsSubClassOf 'E86_ 'E92_ = 'True
  IsSubClassOf 'E86_ 'E1_ = 'True
  IsSubClassOf 'E87_ 'E7_ = 'True
  IsSubClassOf 'E87_ 'E5_ = 'True
  IsSubClassOf 'E87_ 'E4_ = 'True
  IsSubClassOf 'E87_ 'E2_ = 'True
  IsSubClassOf 'E87_ 'E92_ = 'True
  IsSubClassOf 'E87_ 'E1_ = 'True

  -- E8 -> E7 -> ... -> E1
  IsSubClassOf 'E96_ 'E8_ = 'True
  IsSubClassOf 'E96_ 'E7_ = 'True
  IsSubClassOf 'E96_ 'E5_ = 'True
  IsSubClassOf 'E96_ 'E4_ = 'True
  IsSubClassOf 'E96_ 'E2_ = 'True
  IsSubClassOf 'E96_ 'E92_ = 'True
  IsSubClassOf 'E96_ 'E1_ = 'True

  -- E11 -> E7 -> ... -> E1
  IsSubClassOf 'E12_ 'E11_ = 'True
  IsSubClassOf 'E12_ 'E7_ = 'True
  IsSubClassOf 'E12_ 'E5_ = 'True
  IsSubClassOf 'E12_ 'E4_ = 'True
  IsSubClassOf 'E12_ 'E2_ = 'True
  IsSubClassOf 'E12_ 'E92_ = 'True
  IsSubClassOf 'E12_ 'E1_ = 'True
  IsSubClassOf 'E79_ 'E11_ = 'True
  IsSubClassOf 'E79_ 'E7_ = 'True
  IsSubClassOf 'E79_ 'E5_ = 'True
  IsSubClassOf 'E79_ 'E4_ = 'True
  IsSubClassOf 'E79_ 'E2_ = 'True
  IsSubClassOf 'E79_ 'E92_ = 'True
  IsSubClassOf 'E79_ 'E1_ = 'True
  IsSubClassOf 'E80_ 'E11_ = 'True
  IsSubClassOf 'E80_ 'E7_ = 'True
  IsSubClassOf 'E80_ 'E5_ = 'True
  IsSubClassOf 'E80_ 'E4_ = 'True
  IsSubClassOf 'E80_ 'E2_ = 'True
  IsSubClassOf 'E80_ 'E92_ = 'True
  IsSubClassOf 'E80_ 'E1_ = 'True

  -- E12 -> E11 -> E7 -> ... -> E1
  -- E12 -> E63 -> E5 -> ... -> E1
  IsSubClassOf 'E12_ 'E63_ = 'True -- Multiple inheritance

  -- E13 -> E7 -> ... -> E1
  IsSubClassOf 'E14_ 'E13_ = 'True
  IsSubClassOf 'E14_ 'E7_ = 'True
  IsSubClassOf 'E14_ 'E5_ = 'True
  IsSubClassOf 'E14_ 'E4_ = 'True
  IsSubClassOf 'E14_ 'E2_ = 'True
  IsSubClassOf 'E14_ 'E92_ = 'True
  IsSubClassOf 'E14_ 'E1_ = 'True
  IsSubClassOf 'E15_ 'E13_ = 'True
  IsSubClassOf 'E15_ 'E7_ = 'True
  IsSubClassOf 'E15_ 'E5_ = 'True
  IsSubClassOf 'E15_ 'E4_ = 'True
  IsSubClassOf 'E15_ 'E2_ = 'True
  IsSubClassOf 'E15_ 'E92_ = 'True
  IsSubClassOf 'E15_ 'E1_ = 'True
  IsSubClassOf 'E16_ 'E13_ = 'True
  IsSubClassOf 'E16_ 'E7_ = 'True
  IsSubClassOf 'E16_ 'E5_ = 'True
  IsSubClassOf 'E16_ 'E4_ = 'True
  IsSubClassOf 'E16_ 'E2_ = 'True
  IsSubClassOf 'E16_ 'E92_ = 'True
  IsSubClassOf 'E16_ 'E1_ = 'True
  IsSubClassOf 'E17_ 'E13_ = 'True
  IsSubClassOf 'E17_ 'E7_ = 'True
  IsSubClassOf 'E17_ 'E5_ = 'True
  IsSubClassOf 'E17_ 'E4_ = 'True
  IsSubClassOf 'E17_ 'E2_ = 'True
  IsSubClassOf 'E17_ 'E92_ = 'True
  IsSubClassOf 'E17_ 'E1_ = 'True

  -- E63 -> E5 -> ... -> E1
  IsSubClassOf 'E65_ 'E63_ = 'True
  IsSubClassOf 'E65_ 'E5_ = 'True
  IsSubClassOf 'E65_ 'E4_ = 'True
  IsSubClassOf 'E65_ 'E2_ = 'True
  IsSubClassOf 'E65_ 'E92_ = 'True
  IsSubClassOf 'E65_ 'E1_ = 'True
  IsSubClassOf 'E66_ 'E63_ = 'True
  IsSubClassOf 'E66_ 'E5_ = 'True
  IsSubClassOf 'E66_ 'E4_ = 'True
  IsSubClassOf 'E66_ 'E2_ = 'True
  IsSubClassOf 'E66_ 'E92_ = 'True
  IsSubClassOf 'E66_ 'E1_ = 'True
  IsSubClassOf 'E67_ 'E63_ = 'True
  IsSubClassOf 'E67_ 'E5_ = 'True
  IsSubClassOf 'E67_ 'E4_ = 'True
  IsSubClassOf 'E67_ 'E2_ = 'True
  IsSubClassOf 'E67_ 'E92_ = 'True
  IsSubClassOf 'E67_ 'E1_ = 'True
  IsSubClassOf 'E81_ 'E63_ = 'True
  IsSubClassOf 'E81_ 'E5_ = 'True
  IsSubClassOf 'E81_ 'E4_ = 'True
  IsSubClassOf 'E81_ 'E2_ = 'True
  IsSubClassOf 'E81_ 'E92_ = 'True
  IsSubClassOf 'E81_ 'E1_ = 'True

  -- E64 -> E5 -> ... -> E1
  IsSubClassOf 'E6_ 'E64_ = 'True
  IsSubClassOf 'E6_ 'E5_ = 'True
  IsSubClassOf 'E6_ 'E4_ = 'True
  IsSubClassOf 'E6_ 'E2_ = 'True
  IsSubClassOf 'E6_ 'E92_ = 'True
  IsSubClassOf 'E6_ 'E1_ = 'True
  IsSubClassOf 'E68_ 'E64_ = 'True
  IsSubClassOf 'E68_ 'E5_ = 'True
  IsSubClassOf 'E68_ 'E4_ = 'True
  IsSubClassOf 'E68_ 'E2_ = 'True
  IsSubClassOf 'E68_ 'E92_ = 'True
  IsSubClassOf 'E68_ 'E1_ = 'True
  IsSubClassOf 'E69_ 'E64_ = 'True
  IsSubClassOf 'E69_ 'E5_ = 'True
  IsSubClassOf 'E69_ 'E4_ = 'True
  IsSubClassOf 'E69_ 'E2_ = 'True
  IsSubClassOf 'E69_ 'E92_ = 'True
  IsSubClassOf 'E69_ 'E1_ = 'True
  IsSubClassOf 'E81_ 'E64_ = 'True -- Multiple inheritance

  -- E65 -> E7 -> ... -> E1
  -- E65 -> E63 -> E5 -> ... -> E1
  IsSubClassOf 'E65_ 'E7_ = 'True -- Multiple inheritance
  IsSubClassOf 'E83_ 'E65_ = 'True
  IsSubClassOf 'E83_ 'E7_ = 'True
  IsSubClassOf 'E83_ 'E63_ = 'True
  IsSubClassOf 'E83_ 'E5_ = 'True
  IsSubClassOf 'E83_ 'E4_ = 'True
  IsSubClassOf 'E83_ 'E2_ = 'True
  IsSubClassOf 'E83_ 'E92_ = 'True
  IsSubClassOf 'E83_ 'E1_ = 'True

  -- E66 -> E7 -> ... -> E1
  -- E66 -> E63 -> E5 -> ... -> E1
  IsSubClassOf 'E66_ 'E7_ = 'True -- Multiple inheritance

  -- E77 -> E1
  IsSubClassOf 'E39_ 'E77_ = 'True
  IsSubClassOf 'E39_ 'E1_ = 'True
  IsSubClassOf 'E70_ 'E77_ = 'True
  IsSubClassOf 'E70_ 'E1_ = 'True

  -- E39 -> E77 -> E1
  IsSubClassOf 'E21_ 'E39_ = 'True
  IsSubClassOf 'E21_ 'E77_ = 'True
  IsSubClassOf 'E21_ 'E1_ = 'True
  IsSubClassOf 'E74_ 'E39_ = 'True
  IsSubClassOf 'E74_ 'E77_ = 'True
  IsSubClassOf 'E74_ 'E1_ = 'True

  -- E70 -> E77 -> E1
  IsSubClassOf 'E18_ 'E70_ = 'True
  IsSubClassOf 'E18_ 'E77_ = 'True
  IsSubClassOf 'E18_ 'E1_ = 'True
  IsSubClassOf 'E71_ 'E70_ = 'True
  IsSubClassOf 'E71_ 'E77_ = 'True
  IsSubClassOf 'E71_ 'E1_ = 'True
  IsSubClassOf 'E72_ 'E70_ = 'True
  IsSubClassOf 'E72_ 'E77_ = 'True
  IsSubClassOf 'E72_ 'E1_ = 'True

  -- E18 -> E70 -> E77 -> E1
  -- E18 -> E72 -> E70 -> E77 -> E1
  IsSubClassOf 'E18_ 'E72_ = 'True -- Multiple inheritance
  IsSubClassOf 'E19_ 'E18_ = 'True
  IsSubClassOf 'E19_ 'E72_ = 'True
  IsSubClassOf 'E19_ 'E70_ = 'True
  IsSubClassOf 'E19_ 'E77_ = 'True
  IsSubClassOf 'E19_ 'E1_ = 'True
  IsSubClassOf 'E26_ 'E18_ = 'True
  IsSubClassOf 'E26_ 'E72_ = 'True
  IsSubClassOf 'E26_ 'E70_ = 'True
  IsSubClassOf 'E26_ 'E77_ = 'True
  IsSubClassOf 'E26_ 'E1_ = 'True

  -- E19 -> E18 -> ... -> E1
  IsSubClassOf 'E20_ 'E19_ = 'True
  IsSubClassOf 'E20_ 'E18_ = 'True
  IsSubClassOf 'E20_ 'E72_ = 'True
  IsSubClassOf 'E20_ 'E70_ = 'True
  IsSubClassOf 'E20_ 'E77_ = 'True
  IsSubClassOf 'E20_ 'E1_ = 'True
  IsSubClassOf 'E22_ 'E19_ = 'True
  IsSubClassOf 'E22_ 'E18_ = 'True
  IsSubClassOf 'E22_ 'E72_ = 'True
  IsSubClassOf 'E22_ 'E70_ = 'True
  IsSubClassOf 'E22_ 'E77_ = 'True
  IsSubClassOf 'E22_ 'E1_ = 'True

  -- E20 -> E19 -> ... -> E1
  IsSubClassOf 'E21_ 'E20_ = 'True -- Multiple inheritance
  IsSubClassOf 'E21_ 'E19_ = 'True
  IsSubClassOf 'E21_ 'E18_ = 'True
  IsSubClassOf 'E21_ 'E72_ = 'True
  IsSubClassOf 'E21_ 'E70_ = 'True

  -- E26 -> E18 -> ... -> E1
  IsSubClassOf 'E25_ 'E26_ = 'True
  IsSubClassOf 'E25_ 'E18_ = 'True
  IsSubClassOf 'E25_ 'E72_ = 'True
  IsSubClassOf 'E25_ 'E70_ = 'True
  IsSubClassOf 'E25_ 'E77_ = 'True
  IsSubClassOf 'E25_ 'E1_ = 'True
  IsSubClassOf 'E27_ 'E26_ = 'True
  IsSubClassOf 'E27_ 'E18_ = 'True
  IsSubClassOf 'E27_ 'E72_ = 'True
  IsSubClassOf 'E27_ 'E70_ = 'True
  IsSubClassOf 'E27_ 'E77_ = 'True
  IsSubClassOf 'E27_ 'E1_ = 'True

  -- E71 -> E70 -> E77 -> E1
  IsSubClassOf 'E24_ 'E71_ = 'True
  IsSubClassOf 'E24_ 'E70_ = 'True
  IsSubClassOf 'E24_ 'E77_ = 'True
  IsSubClassOf 'E24_ 'E1_ = 'True
  IsSubClassOf 'E28_ 'E71_ = 'True
  IsSubClassOf 'E28_ 'E70_ = 'True
  IsSubClassOf 'E28_ 'E77_ = 'True
  IsSubClassOf 'E28_ 'E1_ = 'True

  -- E24 -> E71 -> E70 -> E77 -> E1
  -- E24 -> E18 -> E72 -> E70 -> E77 -> E1
  IsSubClassOf 'E24_ 'E18_ = 'True -- Multiple inheritance
  IsSubClassOf 'E24_ 'E72_ = 'True
  IsSubClassOf 'E22_ 'E24_ = 'True -- Multiple inheritance
  IsSubClassOf 'E22_ 'E71_ = 'True
  IsSubClassOf 'E25_ 'E24_ = 'True -- Multiple inheritance
  IsSubClassOf 'E25_ 'E71_ = 'True
  IsSubClassOf 'E78_ 'E24_ = 'True -- Multiple inheritance
  IsSubClassOf 'E78_ 'E71_ = 'True

  -- E28 -> E71 -> E70 -> E77 -> E1
  IsSubClassOf 'E55_ 'E28_ = 'True
  IsSubClassOf 'E55_ 'E71_ = 'True
  IsSubClassOf 'E55_ 'E70_ = 'True
  IsSubClassOf 'E55_ 'E77_ = 'True
  IsSubClassOf 'E55_ 'E1_ = 'True
  IsSubClassOf 'E89_ 'E28_ = 'True
  IsSubClassOf 'E89_ 'E71_ = 'True
  IsSubClassOf 'E89_ 'E70_ = 'True
  IsSubClassOf 'E89_ 'E77_ = 'True
  IsSubClassOf 'E89_ 'E1_ = 'True
  IsSubClassOf 'E90_ 'E28_ = 'True
  IsSubClassOf 'E90_ 'E71_ = 'True
  IsSubClassOf 'E90_ 'E70_ = 'True
  IsSubClassOf 'E90_ 'E77_ = 'True
  IsSubClassOf 'E90_ 'E1_ = 'True

  -- E55 -> E28 -> ... -> E1
  IsSubClassOf 'E56_ 'E55_ = 'True
  IsSubClassOf 'E56_ 'E28_ = 'True
  IsSubClassOf 'E56_ 'E71_ = 'True
  IsSubClassOf 'E56_ 'E70_ = 'True
  IsSubClassOf 'E56_ 'E77_ = 'True
  IsSubClassOf 'E56_ 'E1_ = 'True
  IsSubClassOf 'E57_ 'E55_ = 'True
  IsSubClassOf 'E57_ 'E28_ = 'True
  IsSubClassOf 'E57_ 'E71_ = 'True
  IsSubClassOf 'E57_ 'E70_ = 'True
  IsSubClassOf 'E57_ 'E77_ = 'True
  IsSubClassOf 'E57_ 'E1_ = 'True
  IsSubClassOf 'E58_ 'E55_ = 'True
  IsSubClassOf 'E58_ 'E28_ = 'True
  IsSubClassOf 'E58_ 'E71_ = 'True
  IsSubClassOf 'E58_ 'E70_ = 'True
  IsSubClassOf 'E58_ 'E77_ = 'True
  IsSubClassOf 'E58_ 'E1_ = 'True
  IsSubClassOf 'E99_ 'E55_ = 'True
  IsSubClassOf 'E99_ 'E28_ = 'True
  IsSubClassOf 'E99_ 'E71_ = 'True
  IsSubClassOf 'E99_ 'E70_ = 'True
  IsSubClassOf 'E99_ 'E77_ = 'True
  IsSubClassOf 'E99_ 'E1_ = 'True

  -- E89 -> E28 -> ... -> E1
  IsSubClassOf 'E30_ 'E89_ = 'True
  IsSubClassOf 'E30_ 'E28_ = 'True
  IsSubClassOf 'E30_ 'E71_ = 'True
  IsSubClassOf 'E30_ 'E70_ = 'True
  IsSubClassOf 'E30_ 'E77_ = 'True
  IsSubClassOf 'E30_ 'E1_ = 'True
  IsSubClassOf 'E73_ 'E89_ = 'True
  IsSubClassOf 'E73_ 'E28_ = 'True
  IsSubClassOf 'E73_ 'E71_ = 'True
  IsSubClassOf 'E73_ 'E70_ = 'True
  IsSubClassOf 'E73_ 'E77_ = 'True
  IsSubClassOf 'E73_ 'E1_ = 'True

  -- E90 -> E28 -> ... -> E1
  -- E90 -> E72 -> E70 -> ... -> E1
  IsSubClassOf 'E90_ 'E72_ = 'True -- Multiple inheritance
  IsSubClassOf 'E41_ 'E90_ = 'True
  IsSubClassOf 'E41_ 'E28_ = 'True
  IsSubClassOf 'E41_ 'E71_ = 'True
  IsSubClassOf 'E41_ 'E72_ = 'True
  IsSubClassOf 'E41_ 'E70_ = 'True
  IsSubClassOf 'E41_ 'E77_ = 'True
  IsSubClassOf 'E41_ 'E1_ = 'True
  IsSubClassOf 'E73_ 'E90_ = 'True -- Multiple inheritance

  -- E73 -> E89 -> E28 -> ... -> E1
  -- E73 -> E90 -> E28 -> ... -> E1
  -- E73 -> E90 -> E72 -> ... -> E1
  IsSubClassOf 'E29_ 'E73_ = 'True
  IsSubClassOf 'E29_ 'E89_ = 'True
  IsSubClassOf 'E29_ 'E90_ = 'True
  IsSubClassOf 'E29_ 'E28_ = 'True
  IsSubClassOf 'E29_ 'E71_ = 'True
  IsSubClassOf 'E29_ 'E72_ = 'True
  IsSubClassOf 'E29_ 'E70_ = 'True
  IsSubClassOf 'E29_ 'E77_ = 'True
  IsSubClassOf 'E29_ 'E1_ = 'True
  IsSubClassOf 'E31_ 'E73_ = 'True
  IsSubClassOf 'E31_ 'E89_ = 'True
  IsSubClassOf 'E31_ 'E90_ = 'True
  IsSubClassOf 'E31_ 'E28_ = 'True
  IsSubClassOf 'E31_ 'E71_ = 'True
  IsSubClassOf 'E31_ 'E72_ = 'True
  IsSubClassOf 'E31_ 'E70_ = 'True
  IsSubClassOf 'E31_ 'E77_ = 'True
  IsSubClassOf 'E31_ 'E1_ = 'True
  IsSubClassOf 'E33_ 'E73_ = 'True
  IsSubClassOf 'E33_ 'E89_ = 'True
  IsSubClassOf 'E33_ 'E90_ = 'True
  IsSubClassOf 'E33_ 'E28_ = 'True
  IsSubClassOf 'E33_ 'E71_ = 'True
  IsSubClassOf 'E33_ 'E72_ = 'True
  IsSubClassOf 'E33_ 'E70_ = 'True
  IsSubClassOf 'E33_ 'E77_ = 'True
  IsSubClassOf 'E33_ 'E1_ = 'True
  IsSubClassOf 'E36_ 'E73_ = 'True
  IsSubClassOf 'E36_ 'E89_ = 'True
  IsSubClassOf 'E36_ 'E90_ = 'True
  IsSubClassOf 'E36_ 'E28_ = 'True
  IsSubClassOf 'E36_ 'E71_ = 'True
  IsSubClassOf 'E36_ 'E72_ = 'True
  IsSubClassOf 'E36_ 'E70_ = 'True
  IsSubClassOf 'E36_ 'E77_ = 'True
  IsSubClassOf 'E36_ 'E1_ = 'True

  -- E31 -> E73 -> ... -> E1
  IsSubClassOf 'E32_ 'E31_ = 'True
  IsSubClassOf 'E32_ 'E73_ = 'True
  IsSubClassOf 'E32_ 'E89_ = 'True
  IsSubClassOf 'E32_ 'E90_ = 'True
  IsSubClassOf 'E32_ 'E28_ = 'True
  IsSubClassOf 'E32_ 'E71_ = 'True
  IsSubClassOf 'E32_ 'E72_ = 'True
  IsSubClassOf 'E32_ 'E70_ = 'True
  IsSubClassOf 'E32_ 'E77_ = 'True
  IsSubClassOf 'E32_ 'E1_ = 'True

  -- E33 -> E73 -> ... -> E1
  IsSubClassOf 'E33_E41_ 'E33_ = 'True
  IsSubClassOf 'E33_E41_ 'E73_ = 'True
  IsSubClassOf 'E33_E41_ 'E89_ = 'True
  IsSubClassOf 'E33_E41_ 'E90_ = 'True
  IsSubClassOf 'E33_E41_ 'E28_ = 'True
  IsSubClassOf 'E33_E41_ 'E71_ = 'True
  IsSubClassOf 'E33_E41_ 'E72_ = 'True
  IsSubClassOf 'E33_E41_ 'E70_ = 'True
  IsSubClassOf 'E33_E41_ 'E77_ = 'True
  IsSubClassOf 'E33_E41_ 'E1_ = 'True
  IsSubClassOf 'E34_ 'E33_ = 'True
  IsSubClassOf 'E34_ 'E73_ = 'True
  IsSubClassOf 'E34_ 'E89_ = 'True
  IsSubClassOf 'E34_ 'E90_ = 'True
  IsSubClassOf 'E34_ 'E28_ = 'True
  IsSubClassOf 'E34_ 'E71_ = 'True
  IsSubClassOf 'E34_ 'E72_ = 'True
  IsSubClassOf 'E34_ 'E70_ = 'True
  IsSubClassOf 'E34_ 'E77_ = 'True
  IsSubClassOf 'E34_ 'E1_ = 'True
  IsSubClassOf 'E35_ 'E33_ = 'True
  IsSubClassOf 'E35_ 'E73_ = 'True
  IsSubClassOf 'E35_ 'E89_ = 'True
  IsSubClassOf 'E35_ 'E90_ = 'True
  IsSubClassOf 'E35_ 'E28_ = 'True
  IsSubClassOf 'E35_ 'E71_ = 'True
  IsSubClassOf 'E35_ 'E72_ = 'True
  IsSubClassOf 'E35_ 'E70_ = 'True
  IsSubClassOf 'E35_ 'E77_ = 'True
  IsSubClassOf 'E35_ 'E1_ = 'True

  -- E36 -> E73 -> ... -> E1
  IsSubClassOf 'E37_ 'E36_ = 'True
  IsSubClassOf 'E37_ 'E73_ = 'True
  IsSubClassOf 'E37_ 'E89_ = 'True
  IsSubClassOf 'E37_ 'E90_ = 'True
  IsSubClassOf 'E37_ 'E28_ = 'True
  IsSubClassOf 'E37_ 'E71_ = 'True
  IsSubClassOf 'E37_ 'E72_ = 'True
  IsSubClassOf 'E37_ 'E70_ = 'True
  IsSubClassOf 'E37_ 'E77_ = 'True
  IsSubClassOf 'E37_ 'E1_ = 'True

  -- E37 -> E36 -> ... -> E1
  IsSubClassOf 'E34_ 'E37_ = 'True -- Multiple inheritance

  -- E41 -> E90 -> ... -> E1
  IsSubClassOf 'E33_E41_ 'E41_ = 'True -- Multiple inheritance
  IsSubClassOf 'E35_ 'E41_ = 'True -- Multiple inheritance
  IsSubClassOf 'E42_ 'E41_ = 'True
  IsSubClassOf 'E42_ 'E90_ = 'True
  IsSubClassOf 'E42_ 'E28_ = 'True
  IsSubClassOf 'E42_ 'E71_ = 'True
  IsSubClassOf 'E42_ 'E72_ = 'True
  IsSubClassOf 'E42_ 'E70_ = 'True
  IsSubClassOf 'E42_ 'E77_ = 'True
  IsSubClassOf 'E42_ 'E1_ = 'True

  -- E54 -> E1
  IsSubClassOf 'E97_ 'E54_ = 'True
  IsSubClassOf 'E97_ 'E1_ = 'True

  -- E58 -> E55 -> ... -> E1
  IsSubClassOf 'E98_ 'E58_ = 'True
  IsSubClassOf 'E98_ 'E55_ = 'True
  IsSubClassOf 'E98_ 'E28_ = 'True
  IsSubClassOf 'E98_ 'E71_ = 'True
  IsSubClassOf 'E98_ 'E70_ = 'True
  IsSubClassOf 'E98_ 'E77_ = 'True
  IsSubClassOf 'E98_ 'E1_ = 'True

  -- E92 -> E1
  IsSubClassOf 'E4_ 'E92_ = 'True -- Multiple inheritance
  IsSubClassOf 'E93_ 'E92_ = 'True
  IsSubClassOf 'E93_ 'E1_ = 'True

  -- PC14_ (Custom?) -> E1
  IsSubClassOf 'PC14_ 'E1_ = 'True

  -- D1_ -> E1
  IsSubClassOf 'D1_ 'E73_ = 'True
  IsSubClassOf 'D1_ 'E89_ = 'True
  IsSubClassOf 'D1_ 'E28_ = 'True
  IsSubClassOf 'D1_ 'E71_ = 'True
  IsSubClassOf 'D1_ 'E70_ = 'True
  IsSubClassOf 'D1_ 'E77_ = 'True
  IsSubClassOf 'D1_ 'E1_ = 'True

  -- Default
  IsSubClassOf c1 c2 = 'False

-- | A property connecting two CRM classes, ensuring type-safe connections
data Property_ (domain :: Class_) (range :: Class_) (from :: Class_) (to :: Class_) where
  -- | P0 dummy property that we don't render but can use to connect sub graphs in DSL
  P0    :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E1_ 'E1_ src tgt

  -- | P1_is_identified_by
  P1    :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E41_ ~ 'True)
         => Property_ 'E1_ 'E41_ src tgt
  -- | P1i_identifies
  P1i   :: (IsSubClassOf src 'E41_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E41_ 'E1_ src tgt
  -- | P2_has_type
  P2    :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E1_ 'E55_ src tgt
  -- | P2i_is_type_of
  P2i   :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E55_ 'E1_ src tgt
  -- | P3_has_note
  P3    :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E1_ 'Literal_ src tgt
  -- | P4_has_time-span
  P4    :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E52_ ~ 'True)
         => Property_ 'E2_ 'E52_ src tgt
  -- | P4i_is_time-span_of
  P4i   :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E52_ 'E2_ src tgt
  -- | P5_consists_of
  P5    :: (IsSubClassOf src 'E3_ ~ 'True, IsSubClassOf tgt 'E3_ ~ 'True)
         => Property_ 'E3_ 'E3_ src tgt
  -- | P5i_forms_part_of
  P5i   :: (IsSubClassOf src 'E3_ ~ 'True, IsSubClassOf tgt 'E3_ ~ 'True)
         => Property_ 'E3_ 'E3_ src tgt
  -- | P7_took_place_at
  P7    :: (IsSubClassOf src 'E4_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E4_ 'E53_ src tgt
  -- | P7i_witnessed
  P7i   :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E4_ ~ 'True)
         => Property_ 'E53_ 'E4_ src tgt
  -- | P8_took_place_on_or_within
  P8    :: (IsSubClassOf src 'E4_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E4_ 'E18_ src tgt
  -- | P8i_witnessed
  P8i   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E4_ ~ 'True)
         => Property_ 'E18_ 'E4_ src tgt
  -- | P9_consists_of
  P9    :: (IsSubClassOf src 'E4_ ~ 'True, IsSubClassOf tgt 'E4_ ~ 'True)
         => Property_ 'E4_ 'E4_ src tgt
  -- | P9i_forms_part_of
  P9i   :: (IsSubClassOf src 'E4_ ~ 'True, IsSubClassOf tgt 'E4_ ~ 'True)
         => Property_ 'E4_ 'E4_ src tgt
  -- | P10_falls_within
  P10   :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E92_ 'E92_ src tgt
  -- | P10i_contains
  P10i  :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E92_ 'E92_ src tgt
  -- | P11_had_participant
  P11   :: (IsSubClassOf src 'E5_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E5_ 'E39_ src tgt
  -- | P11i_participated_in
  P11i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E5_ ~ 'True)
         => Property_ 'E39_ 'E5_ src tgt
  -- | P12_occurred_in_the_presence_of
  P12   :: (IsSubClassOf src 'E5_ ~ 'True, IsSubClassOf tgt 'E77_ ~ 'True)
         => Property_ 'E5_ 'E77_ src tgt
  -- | P12i_was_present_at
  P12i  :: (IsSubClassOf src 'E77_ ~ 'True, IsSubClassOf tgt 'E5_ ~ 'True)
         => Property_ 'E77_ 'E5_ src tgt
  -- | P13_destroyed
  P13   :: (IsSubClassOf src 'E6_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E6_ 'E18_ src tgt
  -- | P13i_was_destroyed_by
  P13i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E6_ ~ 'True)
         => Property_ 'E18_ 'E6_ src tgt
  -- | P14_carried_out_by
  P14   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E7_ 'E39_ src tgt
  -- | P14i_performed
  P14i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E39_ 'E7_ src tgt
  -- | P15_was_influenced_by
  P15   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E7_ 'E1_ src tgt
  -- | P15i_influenced
  P15i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E1_ 'E7_ src tgt
  -- | P16_used_specific_object
  P16   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E70_ ~ 'True)
         => Property_ 'E7_ 'E70_ src tgt
  -- | P16i_was_used_for
  P16i  :: (IsSubClassOf src 'E70_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E70_ 'E7_ src tgt
  -- | P17_was_motivated_by
  P17   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E7_ 'E1_ src tgt
  -- | P17i_motivated
  P17i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E1_ 'E7_ src tgt
  -- | P19_was_intended_use_of
  P19   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E71_ ~ 'True)
         => Property_ 'E7_ 'E71_ src tgt
  -- | P19i_was_made_for
  P19i  :: (IsSubClassOf src 'E71_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E71_ 'E7_ src tgt
  -- | P20_had_specific_purpose
  P20   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E5_ ~ 'True)
         => Property_ 'E7_ 'E5_ src tgt
  -- | P20i_was_purpose_of
  P20i  :: (IsSubClassOf src 'E5_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E5_ 'E7_ src tgt
  -- | P21_had_general_purpose
  P21   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E7_ 'E55_ src tgt
  -- | P21i_was_purpose_of
  P21i  :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E55_ 'E7_ src tgt
  -- | P22_transferred_title_to
  P22   :: (IsSubClassOf src 'E8_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E8_ 'E39_ src tgt
  -- | P22i_acquired_title_through
  P22i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E8_ ~ 'True)
         => Property_ 'E39_ 'E8_ src tgt
  -- | P23_transferred_title_from
  P23   :: (IsSubClassOf src 'E8_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E8_ 'E39_ src tgt
  -- | P23i_surrendered_title_through
  P23i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E8_ ~ 'True)
         => Property_ 'E39_ 'E8_ src tgt
  -- | P24_transferred_title_of
  P24   :: (IsSubClassOf src 'E8_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E8_ 'E18_ src tgt
  -- | P24i_changed_ownership_through
  P24i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E8_ ~ 'True)
         => Property_ 'E18_ 'E8_ src tgt
  -- | P25_moved
  P25   :: (IsSubClassOf src 'E9_ ~ 'True, IsSubClassOf tgt 'E19_ ~ 'True)
         => Property_ 'E9_ 'E19_ src tgt
  -- | P25i_moved_by
  P25i  :: (IsSubClassOf src 'E19_ ~ 'True, IsSubClassOf tgt 'E9_ ~ 'True)
         => Property_ 'E19_ 'E9_ src tgt
  -- | P26_moved_to
  P26   :: (IsSubClassOf src 'E9_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E9_ 'E53_ src tgt
  -- | P26i_was_destination_of
  P26i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E9_ ~ 'True)
         => Property_ 'E53_ 'E9_ src tgt
  -- | P27_moved_from
  P27   :: (IsSubClassOf src 'E9_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E9_ 'E53_ src tgt
  -- | P27i_was_origin_of
  P27i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E9_ ~ 'True)
         => Property_ 'E53_ 'E9_ src tgt
  -- | P28_custody_surrendered_by
  P28   :: (IsSubClassOf src 'E10_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E10_ 'E39_ src tgt
  -- | P28i_surrendered_custody_through
  P28i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E10_ ~ 'True)
         => Property_ 'E39_ 'E10_ src tgt
  -- | P29_custody_received_by
  P29   :: (IsSubClassOf src 'E10_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E10_ 'E39_ src tgt
  -- | P29i_received_custody_through
  P29i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E10_ ~ 'True)
         => Property_ 'E39_ 'E10_ src tgt
  -- | P30_transferred_custody_of
  P30   :: (IsSubClassOf src 'E10_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E10_ 'E18_ src tgt
  -- | P30i_custody_transferred_through
  P30i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E10_ ~ 'True)
         => Property_ 'E18_ 'E10_ src tgt
  -- | P31_has_modified
  P31   :: (IsSubClassOf src 'E11_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E11_ 'E18_ src tgt
  -- | P31i_was_modified_by
  P31i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E11_ ~ 'True)
         => Property_ 'E18_ 'E11_ src tgt
  -- | P32_used_general_technique
  P32   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E7_ 'E55_ src tgt
  -- | P32i_was_technique_of
  P32i  :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E55_ 'E7_ src tgt
  -- | P33_used_specific_technique
  P33   :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E29_ ~ 'True)
         => Property_ 'E7_ 'E29_ src tgt
  -- | P33i_was_used_by
  P33i  :: (IsSubClassOf src 'E29_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E29_ 'E7_ src tgt
  -- | P34_concerned
  P34   :: (IsSubClassOf src 'E14_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E14_ 'E18_ src tgt
  -- | P34i_was_assessed_by
  P34i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E14_ ~ 'True)
         => Property_ 'E18_ 'E14_ src tgt
  -- | P35_has_identified
  P35   :: (IsSubClassOf src 'E14_ ~ 'True, IsSubClassOf tgt 'E3_ ~ 'True)
         => Property_ 'E14_ 'E3_ src tgt
  -- | P35i_was_identified_by
  P35i  :: (IsSubClassOf src 'E3_ ~ 'True, IsSubClassOf tgt 'E14_ ~ 'True)
         => Property_ 'E3_ 'E14_ src tgt
  -- | P37_assigned
  P37   :: (IsSubClassOf src 'E15_ ~ 'True, IsSubClassOf tgt 'E42_ ~ 'True)
         => Property_ 'E15_ 'E42_ src tgt
  -- | P37i_was_assigned_by
  P37i  :: (IsSubClassOf src 'E42_ ~ 'True, IsSubClassOf tgt 'E15_ ~ 'True)
         => Property_ 'E42_ 'E15_ src tgt
  -- | P38_deassigned
  P38   :: (IsSubClassOf src 'E15_ ~ 'True, IsSubClassOf tgt 'E42_ ~ 'True)
         => Property_ 'E15_ 'E42_ src tgt
  -- | P38i_was_deassigned_by
  P38i  :: (IsSubClassOf src 'E42_ ~ 'True, IsSubClassOf tgt 'E15_ ~ 'True)
         => Property_ 'E42_ 'E15_ src tgt
  -- | P39_measured
  P39   :: (IsSubClassOf src 'E16_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E16_ 'E18_ src tgt
  -- | P39i_was_measured_by
  P39i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E16_ ~ 'True)
         => Property_ 'E18_ 'E16_ src tgt
  -- | P40_observed_dimension
  P40   :: (IsSubClassOf src 'E16_ ~ 'True, IsSubClassOf tgt 'E54_ ~ 'True)
         => Property_ 'E16_ 'E54_ src tgt
  -- | P40i_was_observed_in
  P40i  :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'E16_ ~ 'True)
         => Property_ 'E54_ 'E16_ src tgt
  -- | P41_classified
  P41   :: (IsSubClassOf src 'E17_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E17_ 'E1_ src tgt
  -- | P41i_was_classified_by
  P41i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E17_ ~ 'True)
         => Property_ 'E1_ 'E17_ src tgt
  -- | P42_assigned
  P42   :: (IsSubClassOf src 'E17_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E17_ 'E55_ src tgt
  -- | P42i_was_assigned_by
  P42i  :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E17_ ~ 'True)
         => Property_ 'E55_ 'E17_ src tgt
  -- | P43_has_dimension
  P43   :: (IsSubClassOf src 'E70_ ~ 'True, IsSubClassOf tgt 'E54_ ~ 'True)
         => Property_ 'E70_ 'E54_ src tgt
  -- | P43i_is_dimension_of
  P43i  :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'E70_ ~ 'True)
         => Property_ 'E54_ 'E70_ src tgt
  -- | P44_has_condition
  P44   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E3_ ~ 'True)
         => Property_ 'E18_ 'E3_ src tgt
  -- | P44i_is_condition_of
  P44i  :: (IsSubClassOf src 'E3_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E3_ 'E18_ src tgt
  -- | P45_consists_of
  P45   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E18_ 'E55_ src tgt
  -- | P45i_is_incorporated_in
  P45i  :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E55_ 'E18_ src tgt
  -- | P46_is_composed_of
  P46   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E18_ 'E18_ src tgt
  -- | P46i_forms_part_of
  P46i  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E18_ 'E18_ src tgt
  -- | P48_has_preferred_identifier
  P48   :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E42_ ~ 'True)
         => Property_ 'E1_ 'E42_ src tgt
  -- | P48i_is_preferred_identifier_of
  P48i  :: (IsSubClassOf src 'E42_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E42_ 'E1_ src tgt
  -- | P49_has_former_or_current_keeper
  P49   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E18_ 'E39_ src tgt
  -- | P49i_is_former_or_current_keeper_of
  P49i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E39_ 'E18_ src tgt
  -- | P50_has_current_keeper
  P50   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E18_ 'E39_ src tgt
  -- | P50i_is_current_keeper_of
  P50i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E39_ 'E18_ src tgt
  -- | P51_has_former_or_current_owner
  P51   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E18_ 'E39_ src tgt
  -- | P51i_is_former_or_current_owner_of
  P51i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E39_ 'E18_ src tgt
  -- | P52_has_current_owner
  P52   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E18_ 'E39_ src tgt
  -- | P52i_is_current_owner_of
  P52i  :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E39_ 'E18_ src tgt
  -- | P53_has_former_or_current_location
  P53   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E18_ 'E53_ src tgt
  -- | P53i_is_former_or_current_location_of
  P53i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E53_ 'E18_ src tgt
  -- | P54_has_current_permanent_location
  P54   :: (IsSubClassOf src 'E19_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E19_ 'E53_ src tgt
  -- | P54i_is_current_permanent_location_of
  P54i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E19_ ~ 'True)
         => Property_ 'E53_ 'E19_ src tgt
  -- | P55_has_current_location
  P55   :: (IsSubClassOf src 'E19_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E19_ 'E53_ src tgt
  -- | P55i_currently_holds
  P55i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E19_ ~ 'True)
         => Property_ 'E53_ 'E19_ src tgt
  -- | P56_bears_feature
  P56   :: (IsSubClassOf src 'E19_ ~ 'True, IsSubClassOf tgt 'E26_ ~ 'True)
         => Property_ 'E19_ 'E26_ src tgt
  -- | P56i_is_found_on
  P56i  :: (IsSubClassOf src 'E26_ ~ 'True, IsSubClassOf tgt 'E19_ ~ 'True)
         => Property_ 'E26_ 'E19_ src tgt
  -- | P57_has_number_of_parts
  P57   :: (IsSubClassOf src 'E19_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E19_ 'Literal_ src tgt
  -- | P59_has_section
  P59   :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E18_ 'E53_ src tgt
  -- | P59i_is_located_on_or_within
  P59i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E53_ 'E18_ src tgt
  -- | P62_depicts
  P62   :: (IsSubClassOf src 'E24_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E24_ 'E1_ src tgt
  -- | P62i_is_depicted_by
  P62i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E24_ ~ 'True)
         => Property_ 'E1_ 'E24_ src tgt
  -- | P65_shows_visual_item
  P65   :: (IsSubClassOf src 'E24_ ~ 'True, IsSubClassOf tgt 'E36_ ~ 'True)
         => Property_ 'E24_ 'E36_ src tgt
  -- | P65i_is_shown_by
  P65i  :: (IsSubClassOf src 'E36_ ~ 'True, IsSubClassOf tgt 'E24_ ~ 'True)
         => Property_ 'E36_ 'E24_ src tgt
  -- | P67_refers_to
  P67   :: (IsSubClassOf src 'E89_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E89_ 'E1_ src tgt
  -- | P67i_is_referred_to_by
  P67i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E89_ ~ 'True)
         => Property_ 'E1_ 'E89_ src tgt
  -- | P68_foresees_use_of
  P68   :: (IsSubClassOf src 'E29_ ~ 'True, IsSubClassOf tgt 'E57_ ~ 'True)
         => Property_ 'E29_ 'E57_ src tgt
  -- | P68i_use_foreseen_by
  P68i  :: (IsSubClassOf src 'E57_ ~ 'True, IsSubClassOf tgt 'E29_ ~ 'True)
         => Property_ 'E57_ 'E29_ src tgt
  -- | P69_has_association_with
  P69   :: (IsSubClassOf src 'E29_ ~ 'True, IsSubClassOf tgt 'E29_ ~ 'True)
         => Property_ 'E29_ 'E29_ src tgt
  -- | P69i_is_associated_with
  P69i  :: (IsSubClassOf src 'E29_ ~ 'True, IsSubClassOf tgt 'E29_ ~ 'True)
         => Property_ 'E29_ 'E29_ src tgt
  -- | P70_documents
  P70   :: (IsSubClassOf src 'E31_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E31_ 'E1_ src tgt
  -- | P70i_is_documented_in
  P70i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E31_ ~ 'True)
         => Property_ 'E1_ 'E31_ src tgt
  -- | P71_lists
  P71   :: (IsSubClassOf src 'E32_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E32_ 'E1_ src tgt
  -- | P71i_is_listed_in
  P71i  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E32_ ~ 'True)
         => Property_ 'E1_ 'E32_ src tgt
  -- | P72_has_language
  P72   :: (IsSubClassOf src 'E33_ ~ 'True, IsSubClassOf tgt 'E56_ ~ 'True)
         => Property_ 'E33_ 'E56_ src tgt
  -- | P72i_is_language_of
  P72i  :: (IsSubClassOf src 'E56_ ~ 'True, IsSubClassOf tgt 'E33_ ~ 'True)
         => Property_ 'E56_ 'E33_ src tgt
  -- | P73_has_translation
  P73   :: (IsSubClassOf src 'E33_ ~ 'True, IsSubClassOf tgt 'E33_ ~ 'True)
         => Property_ 'E33_ 'E33_ src tgt
  -- | P73i_is_translation_of
  P73i  :: (IsSubClassOf src 'E33_ ~ 'True, IsSubClassOf tgt 'E33_ ~ 'True)
         => Property_ 'E33_ 'E33_ src tgt
  -- | P74_has_current_or_former_residence
  P74   :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E39_ 'E53_ src tgt
  -- | P74i_is_current_or_former_residence_of
  P74i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E53_ 'E39_ src tgt
  -- | P75_possesses
  P75   :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E30_ ~ 'True)
         => Property_ 'E39_ 'E30_ src tgt
  -- | P75i_is_possessed_by
  P75i  :: (IsSubClassOf src 'E30_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E30_ 'E39_ src tgt
  -- | P76_has_contact_point
  P76   :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E41_ ~ 'True)
         => Property_ 'E39_ 'E41_ src tgt
  -- | P76i_provides_access_to
  P76i  :: (IsSubClassOf src 'E41_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E41_ 'E39_ src tgt
  -- | P79_beginning_is_qualified_by
  P79   :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P80_end_is_qualified_by
  P80   :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P81_ongoing_throughout
  P81   :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P81a_end_of_the_begin
  P81a  :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P81b_begin_of_the_end
  P81b  :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P82_at_some_time_within
  P82   :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P82a_begin_of_the_begin
  P82a  :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P82b_end_of_the_end
  P82b  :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P86_falls_within
  P86   :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'E52_ ~ 'True)
         => Property_ 'E52_ 'E52_ src tgt
  -- | P86i_contains
  P86i  :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'E52_ ~ 'True)
         => Property_ 'E52_ 'E52_ src tgt
  -- | P89_falls_within
  P89   :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E53_ 'E53_ src tgt
  -- | P89i_contains
  P89i  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E53_ 'E53_ src tgt
  -- | P90_has_value
  P90   :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E54_ 'Literal_ src tgt
  -- | P90a_has_lower_value_limit
  P90a  :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E54_ 'Literal_ src tgt
  -- | P90b_has_upper_value_limit
  P90b  :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E54_ 'Literal_ src tgt
  -- | P91_has_unit
  P91   :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'E58_ ~ 'True)
         => Property_ 'E54_ 'E58_ src tgt
  -- | P91i_is_unit_of
  P91i  :: (IsSubClassOf src 'E58_ ~ 'True, IsSubClassOf tgt 'E54_ ~ 'True)
         => Property_ 'E58_ 'E54_ src tgt
  -- | P92_brought_into_existence
  P92   :: (IsSubClassOf src 'E63_ ~ 'True, IsSubClassOf tgt 'E77_ ~ 'True)
         => Property_ 'E63_ 'E77_ src tgt
  -- | P92i_was_brought_into_existence_by
  P92i  :: (IsSubClassOf src 'E77_ ~ 'True, IsSubClassOf tgt 'E63_ ~ 'True)
         => Property_ 'E77_ 'E63_ src tgt
  -- | P93_took_out_of_existence
  P93   :: (IsSubClassOf src 'E64_ ~ 'True, IsSubClassOf tgt 'E77_ ~ 'True)
         => Property_ 'E64_ 'E77_ src tgt
  -- | P93i_was_taken_out_of_existence_by
  P93i  :: (IsSubClassOf src 'E77_ ~ 'True, IsSubClassOf tgt 'E64_ ~ 'True)
         => Property_ 'E77_ 'E64_ src tgt
  -- | P94_has_created
  P94   :: (IsSubClassOf src 'E65_ ~ 'True, IsSubClassOf tgt 'E28_ ~ 'True)
         => Property_ 'E65_ 'E28_ src tgt
  -- | P94i_was_created_by
  P94i  :: (IsSubClassOf src 'E28_ ~ 'True, IsSubClassOf tgt 'E65_ ~ 'True)
         => Property_ 'E28_ 'E65_ src tgt
  -- | P95_has_formed
  P95   :: (IsSubClassOf src 'E66_ ~ 'True, IsSubClassOf tgt 'E74_ ~ 'True)
         => Property_ 'E66_ 'E74_ src tgt
  -- | P95i_was_formed_by
  P95i  :: (IsSubClassOf src 'E74_ ~ 'True, IsSubClassOf tgt 'E66_ ~ 'True)
         => Property_ 'E74_ 'E66_ src tgt
  -- | P96_by_mother
  P96   :: (IsSubClassOf src 'E67_ ~ 'True, IsSubClassOf tgt 'E21_ ~ 'True)
         => Property_ 'E67_ 'E21_ src tgt
  -- | P96i_gave_birth
  P96i  :: (IsSubClassOf src 'E21_ ~ 'True, IsSubClassOf tgt 'E67_ ~ 'True)
         => Property_ 'E21_ 'E67_ src tgt
  -- | P97_from_father
  P97   :: (IsSubClassOf src 'E67_ ~ 'True, IsSubClassOf tgt 'E21_ ~ 'True)
         => Property_ 'E67_ 'E21_ src tgt
  -- | P97i_was_father_for
  P97i  :: (IsSubClassOf src 'E21_ ~ 'True, IsSubClassOf tgt 'E67_ ~ 'True)
         => Property_ 'E21_ 'E67_ src tgt
  -- | P98_brought_into_life
  P98   :: (IsSubClassOf src 'E67_ ~ 'True, IsSubClassOf tgt 'E21_ ~ 'True)
         => Property_ 'E67_ 'E21_ src tgt
  -- | P98i_was_born
  P98i  :: (IsSubClassOf src 'E21_ ~ 'True, IsSubClassOf tgt 'E67_ ~ 'True)
         => Property_ 'E21_ 'E67_ src tgt
  -- | P99_dissolved
  P99   :: (IsSubClassOf src 'E68_ ~ 'True, IsSubClassOf tgt 'E74_ ~ 'True)
         => Property_ 'E68_ 'E74_ src tgt
  -- | P99i_was_dissolved_by
  P99i  :: (IsSubClassOf src 'E74_ ~ 'True, IsSubClassOf tgt 'E68_ ~ 'True)
         => Property_ 'E74_ 'E68_ src tgt
  -- | P100_was_death_of
  P100  :: (IsSubClassOf src 'E69_ ~ 'True, IsSubClassOf tgt 'E21_ ~ 'True)
         => Property_ 'E69_ 'E21_ src tgt
  -- | P100i_died_in
  P100i :: (IsSubClassOf src 'E21_ ~ 'True, IsSubClassOf tgt 'E69_ ~ 'True)
         => Property_ 'E21_ 'E69_ src tgt
  -- | P101_had_as_general_use
  P101  :: (IsSubClassOf src 'E70_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E70_ 'E55_ src tgt
  -- | P101i_was_use_of
  P101i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E70_ ~ 'True)
         => Property_ 'E55_ 'E70_ src tgt
  -- | P102_has_title
  P102  :: (IsSubClassOf src 'E71_ ~ 'True, IsSubClassOf tgt 'E35_ ~ 'True)
         => Property_ 'E71_ 'E35_ src tgt
  -- | P102i_is_title_of
  P102i :: (IsSubClassOf src 'E35_ ~ 'True, IsSubClassOf tgt 'E71_ ~ 'True)
         => Property_ 'E35_ 'E71_ src tgt
  -- | P103_was_intended_for
  P103  :: (IsSubClassOf src 'E71_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E71_ 'E55_ src tgt
  -- | P103i_was_intention_of
  P103i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E71_ ~ 'True)
         => Property_ 'E55_ 'E71_ src tgt
  -- | P104_is_subject_to
  P104  :: (IsSubClassOf src 'E72_ ~ 'True, IsSubClassOf tgt 'E30_ ~ 'True)
         => Property_ 'E72_ 'E30_ src tgt
  -- | P104i_applies_to
  P104i :: (IsSubClassOf src 'E30_ ~ 'True, IsSubClassOf tgt 'E72_ ~ 'True)
         => Property_ 'E30_ 'E72_ src tgt
  -- | P105_right_held_by
  P105  :: (IsSubClassOf src 'E72_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E72_ 'E39_ src tgt
  -- | P105i_has_right_on
  P105i :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E72_ ~ 'True)
         => Property_ 'E39_ 'E72_ src tgt
  -- | P106_is_composed_of
  P106  :: (IsSubClassOf src 'E90_ ~ 'True, IsSubClassOf tgt 'E90_ ~ 'True)
         => Property_ 'E90_ 'E90_ src tgt
  -- | P106i_forms_part_of
  P106i :: (IsSubClassOf src 'E90_ ~ 'True, IsSubClassOf tgt 'E90_ ~ 'True)
         => Property_ 'E90_ 'E90_ src tgt
  -- | P107_has_current_or_former_member
  P107  :: (IsSubClassOf src 'E74_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E74_ 'E39_ src tgt
  -- | P107i_is_current_or_former_member_of
  P107i :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E74_ ~ 'True)
         => Property_ 'E39_ 'E74_ src tgt
  -- | P108_has_produced
  P108  :: (IsSubClassOf src 'E12_ ~ 'True, IsSubClassOf tgt 'E24_ ~ 'True)
         => Property_ 'E12_ 'E24_ src tgt
  -- | P108i_was_produced_by
  P108i :: (IsSubClassOf src 'E24_ ~ 'True, IsSubClassOf tgt 'E12_ ~ 'True)
         => Property_ 'E24_ 'E12_ src tgt
  -- | P109_has_current_or_former_curator
  P109  :: (IsSubClassOf src 'E78_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E78_ 'E39_ src tgt
  -- | P109i_is_current_or_former_curator_of
  P109i :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E78_ ~ 'True)
         => Property_ 'E39_ 'E78_ src tgt
  -- | P110_augmented
  P110  :: (IsSubClassOf src 'E79_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E79_ 'E18_ src tgt
  -- | P110i_was_augmented_by
  P110i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E79_ ~ 'True)
         => Property_ 'E18_ 'E79_ src tgt
  -- | P111_added
  P111  :: (IsSubClassOf src 'E79_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E79_ 'E18_ src tgt
  -- | P111i_was_added_by
  P111i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E79_ ~ 'True)
         => Property_ 'E18_ 'E79_ src tgt
  -- | P112_diminished
  P112  :: (IsSubClassOf src 'E80_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E80_ 'E18_ src tgt
  -- | P112i_was_diminished_by
  P112i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E80_ ~ 'True)
         => Property_ 'E18_ 'E80_ src tgt
  -- | P113_removed
  P113  :: (IsSubClassOf src 'E80_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E80_ 'E18_ src tgt
  -- | P113i_was_removed_by
  P113i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E80_ ~ 'True)
         => Property_ 'E18_ 'E80_ src tgt
  -- | P121_overlaps_with
  P121  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E53_ 'E53_ src tgt
  -- | P122_borders_with
  P122  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E53_ 'E53_ src tgt
  -- | P123_resulted_in
  P123  :: (IsSubClassOf src 'E81_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E81_ 'E18_ src tgt
  -- | P123i_resulted_from
  P123i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E81_ ~ 'True)
         => Property_ 'E18_ 'E81_ src tgt
  -- | P124_transformed
  P124  :: (IsSubClassOf src 'E81_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E81_ 'E18_ src tgt
  -- | P124i_was_transformed_by
  P124i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E81_ ~ 'True)
         => Property_ 'E18_ 'E81_ src tgt
  -- | P125_used_object_of_type
  P125  :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E7_ 'E55_ src tgt
  -- | P125i_was_type_of_object_used_in
  P125i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E55_ 'E7_ src tgt
  -- | P126_employed
  P126  :: (IsSubClassOf src 'E11_ ~ 'True, IsSubClassOf tgt 'E57_ ~ 'True)
         => Property_ 'E11_ 'E57_ src tgt
  -- | P126i_was_employed_in
  P126i :: (IsSubClassOf src 'E57_ ~ 'True, IsSubClassOf tgt 'E11_ ~ 'True)
         => Property_ 'E57_ 'E11_ src tgt
  -- | P127_has_broader_term
  P127  :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E55_ 'E55_ src tgt
  -- | P127i_has_narrower_term
  P127i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E55_ 'E55_ src tgt
  -- | P128_carries
  P128  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E90_ ~ 'True)
         => Property_ 'E18_ 'E90_ src tgt
  -- | P128i_is_carried_by
  P128i :: (IsSubClassOf src 'E90_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E90_ 'E18_ src tgt
  -- | P129_is_about
  P129  :: (IsSubClassOf src 'E89_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E89_ 'E1_ src tgt
  -- | P129i_is_subject_of
  P129i :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E89_ ~ 'True)
         => Property_ 'E1_ 'E89_ src tgt
  -- | P130_shows_features_of
  P130  :: (IsSubClassOf src 'E70_ ~ 'True, IsSubClassOf tgt 'E70_ ~ 'True)
         => Property_ 'E70_ 'E70_ src tgt
  -- | P130i_features_are_also_found_on
  P130i :: (IsSubClassOf src 'E70_ ~ 'True, IsSubClassOf tgt 'E70_ ~ 'True)
         => Property_ 'E70_ 'E70_ src tgt
  -- | P132_spatiotemporally_overlaps_with
  P132  :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E92_ 'E92_ src tgt
  -- | P133_is_spatiotemporally_separated_from
  P133  :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E92_ 'E92_ src tgt
  -- | P134_continued
  P134  :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E7_ 'E7_ src tgt
  -- | P134i_was_continued_by
  P134i :: (IsSubClassOf src 'E7_ ~ 'True, IsSubClassOf tgt 'E7_ ~ 'True)
         => Property_ 'E7_ 'E7_ src tgt
  -- | P135_created_type
  P135  :: (IsSubClassOf src 'E83_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E83_ 'E55_ src tgt
  -- | P135i_was_created_by
  P135i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E83_ ~ 'True)
         => Property_ 'E55_ 'E83_ src tgt
  -- | P136_was_based_on
  P136  :: (IsSubClassOf src 'E83_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E83_ 'E1_ src tgt
  -- | P136i_supported_type_creation
  P136i :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E83_ ~ 'True)
         => Property_ 'E1_ 'E83_ src tgt
  -- | P137_exemplifies
  P137  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E1_ 'E55_ src tgt
  -- | P137i_is_exemplified_by
  P137i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E55_ 'E1_ src tgt
  -- | P138_represents
  P138  :: (IsSubClassOf src 'E36_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E36_ 'E1_ src tgt
  -- | P138i_has_representation
  P138i :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E36_ ~ 'True)
         => Property_ 'E1_ 'E36_ src tgt
  -- | P139_has_alternative_form
  P139  :: (IsSubClassOf src 'E41_ ~ 'True, IsSubClassOf tgt 'E41_ ~ 'True)
         => Property_ 'E41_ 'E41_ src tgt
  -- | P139i_is_alternative_form_of
  P139i :: (IsSubClassOf src 'E41_ ~ 'True, IsSubClassOf tgt 'E41_ ~ 'True)
         => Property_ 'E41_ 'E41_ src tgt
  -- | P140_assigned_attribute_to
  P140  :: (IsSubClassOf src 'E13_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E13_ 'E1_ src tgt
  -- | P140i_was_attributed_by
  P140i :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E13_ ~ 'True)
         => Property_ 'E1_ 'E13_ src tgt
  -- | P141_assigned
  P141  :: (IsSubClassOf src 'E13_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E13_ 'E1_ src tgt
  -- | P141i_was_assigned_by
  P141i :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E13_ ~ 'True)
         => Property_ 'E1_ 'E13_ src tgt
  -- | P142_used_constituent
  P142  :: (IsSubClassOf src 'E15_ ~ 'True, IsSubClassOf tgt 'E90_ ~ 'True)
         => Property_ 'E15_ 'E90_ src tgt
  -- | P142i_was_used_in
  P142i :: (IsSubClassOf src 'E90_ ~ 'True, IsSubClassOf tgt 'E15_ ~ 'True)
         => Property_ 'E90_ 'E15_ src tgt
  -- | P143_joined
  P143  :: (IsSubClassOf src 'E85_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E85_ 'E39_ src tgt
  -- | P143i_was_joined_by
  P143i :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E85_ ~ 'True)
         => Property_ 'E39_ 'E85_ src tgt
  -- | P144_joined_with
  P144  :: (IsSubClassOf src 'E85_ ~ 'True, IsSubClassOf tgt 'E74_ ~ 'True)
         => Property_ 'E85_ 'E74_ src tgt
  -- | P144i_gained_member_by
  P144i :: (IsSubClassOf src 'E74_ ~ 'True, IsSubClassOf tgt 'E85_ ~ 'True)
         => Property_ 'E74_ 'E85_ src tgt
  -- | P145_separated
  P145  :: (IsSubClassOf src 'E86_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'E86_ 'E39_ src tgt
  -- | P145i_left_by
  P145i :: (IsSubClassOf src 'E39_ ~ 'True, IsSubClassOf tgt 'E86_ ~ 'True)
         => Property_ 'E39_ 'E86_ src tgt
  -- | P146_separated_from
  P146  :: (IsSubClassOf src 'E86_ ~ 'True, IsSubClassOf tgt 'E74_ ~ 'True)
         => Property_ 'E86_ 'E74_ src tgt
  -- | P146i_lost_member_by
  P146i :: (IsSubClassOf src 'E74_ ~ 'True, IsSubClassOf tgt 'E86_ ~ 'True)
         => Property_ 'E74_ 'E86_ src tgt
  -- | P147_curated
  P147  :: (IsSubClassOf src 'E87_ ~ 'True, IsSubClassOf tgt 'E78_ ~ 'True)
         => Property_ 'E87_ 'E78_ src tgt
  -- | P147i_was_curated_by
  P147i :: (IsSubClassOf src 'E78_ ~ 'True, IsSubClassOf tgt 'E87_ ~ 'True)
         => Property_ 'E78_ 'E87_ src tgt
  -- | P148_has_component
  P148  :: (IsSubClassOf src 'E89_ ~ 'True, IsSubClassOf tgt 'E89_ ~ 'True)
         => Property_ 'E89_ 'E89_ src tgt
  -- | P148i_is_component_of
  P148i :: (IsSubClassOf src 'E89_ ~ 'True, IsSubClassOf tgt 'E89_ ~ 'True)
         => Property_ 'E89_ 'E89_ src tgt
  -- | P150_defines_typical_parts_of
  P150  :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E55_ 'E55_ src tgt
  -- | P150i_defines_typical_wholes_for
  P150i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E55_ 'E55_ src tgt
  -- | P151_was_formed_from
  P151  :: (IsSubClassOf src 'E66_ ~ 'True, IsSubClassOf tgt 'E74_ ~ 'True)
         => Property_ 'E66_ 'E74_ src tgt
  -- | P151i_participated_in
  P151i :: (IsSubClassOf src 'E74_ ~ 'True, IsSubClassOf tgt 'E66_ ~ 'True)
         => Property_ 'E74_ 'E66_ src tgt
  -- | P152_has_parent
  P152  :: (IsSubClassOf src 'E21_ ~ 'True, IsSubClassOf tgt 'E21_ ~ 'True)
         => Property_ 'E21_ 'E21_ src tgt
  -- | P152i_is_parent_of
  P152i :: (IsSubClassOf src 'E21_ ~ 'True, IsSubClassOf tgt 'E21_ ~ 'True)
         => Property_ 'E21_ 'E21_ src tgt
  -- | P156_occupies
  P156  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E18_ 'E53_ src tgt
  -- | P156i_is_occupied_by
  P156i :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E53_ 'E18_ src tgt
  -- | P157_is_at_rest_relative_to
  P157  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E53_ 'E18_ src tgt
  -- | P157i_provides_reference_space_for
  P157i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E18_ 'E53_ src tgt
  -- | P160_has_temporal_projection
  P160  :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E52_ ~ 'True)
         => Property_ 'E92_ 'E52_ src tgt
  -- | P160i_is_temporal_projection_of
  P160i :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E52_ 'E92_ src tgt
  -- | P161_has_spatial_projection
  P161  :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E92_ 'E53_ src tgt
  -- | P161i_is_spatial_projection_of
  P161i :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E53_ 'E92_ src tgt
  -- | P164_is_temporally_specified_by
  P164  :: (IsSubClassOf src 'E93_ ~ 'True, IsSubClassOf tgt 'E52_ ~ 'True)
         => Property_ 'E93_ 'E52_ src tgt
  -- | P164i_temporally_specifies
  P164i :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'E93_ ~ 'True)
         => Property_ 'E52_ 'E93_ src tgt
  -- | P165_incorporates
  P165  :: (IsSubClassOf src 'E73_ ~ 'True, IsSubClassOf tgt 'E90_ ~ 'True)
         => Property_ 'E73_ 'E90_ src tgt
  -- | P165i_is_incorporated_in
  P165i :: (IsSubClassOf src 'E90_ ~ 'True, IsSubClassOf tgt 'E73_ ~ 'True)
         => Property_ 'E90_ 'E73_ src tgt
  -- | P166_was_a_presence_of
  P166  :: (IsSubClassOf src 'E93_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E93_ 'E92_ src tgt
  -- | P166i_had_presence
  P166i :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E93_ ~ 'True)
         => Property_ 'E92_ 'E93_ src tgt
  -- | P167_was_within
  P167  :: (IsSubClassOf src 'E93_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E93_ 'E53_ src tgt
  -- | P167i_includes
  P167i :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E93_ ~ 'True)
         => Property_ 'E53_ 'E93_ src tgt
  -- | P168_place_is_defined_by
  P168  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E53_ 'Literal_ src tgt
  -- | P169i_spacetime_volume_is_defined_by
  P169i :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E92_ 'Literal_ src tgt
  -- | P170i_time_is_defined_by
  P170i :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E52_ 'Literal_ src tgt
  -- | P171_at_some_place_within
  P171  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E53_ 'Literal_ src tgt
  -- | P172_contains
  P172  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E53_ 'Literal_ src tgt
  -- | P173_starts_before_or_with_the_end_of
  P173  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P173i_ends_after_or_with_the_start_of
  P173i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P174_starts_before_the_end_of
  P174  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P174i_ends_after_the_start_of
  P174i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P175_starts_before_or_with_the_start_of
  P175  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P175i_starts_after_or_with_the_start_of
  P175i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P176_starts_before_the_start_of
  P176  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P176i_starts_after_the_start_of
  P176i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P177_assigned_property_of_type
  P177  :: (IsSubClassOf src 'E13_ ~ 'True, IsSubClassOf tgt 'E55_ ~ 'True)
         => Property_ 'E13_ 'E55_ src tgt
  -- | P177i_is_type_of_property_assigned
  P177i :: (IsSubClassOf src 'E55_ ~ 'True, IsSubClassOf tgt 'E13_ ~ 'True)
         => Property_ 'E55_ 'E13_ src tgt
  -- | P179_had_sales_price
  P179  :: (IsSubClassOf src 'E96_ ~ 'True, IsSubClassOf tgt 'E97_ ~ 'True)
         => Property_ 'E96_ 'E97_ src tgt
  -- | P179i_was_sales_price_of
  P179i :: (IsSubClassOf src 'E97_ ~ 'True, IsSubClassOf tgt 'E96_ ~ 'True)
         => Property_ 'E97_ 'E96_ src tgt
  -- | P180_has_currency
  P180  :: (IsSubClassOf src 'E97_ ~ 'True, IsSubClassOf tgt 'E98_ ~ 'True)
         => Property_ 'E97_ 'E98_ src tgt
  -- | P180i_was_currency_of
  P180i :: (IsSubClassOf src 'E98_ ~ 'True, IsSubClassOf tgt 'E97_ ~ 'True)
         => Property_ 'E98_ 'E97_ src tgt
  -- | P182_ends_before_or_with_the_start_of
  P182  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P182i_starts_after_or_with_the_end_of
  P182i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P183_ends_before_the_start_of
  P183  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P183i_starts_after_the_end_of
  P183i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P184_ends_before_or_with_the_end_of
  P184  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P184i_ends_with_or_after_the_end_of
  P184i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P185_ends_before_the_end_of
  P185  :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P185i_ends_after_the_end_of
  P185i :: (IsSubClassOf src 'E2_ ~ 'True, IsSubClassOf tgt 'E2_ ~ 'True)
         => Property_ 'E2_ 'E2_ src tgt
  -- | P186_produced_thing_of_product_type
  P186  :: (IsSubClassOf src 'E12_ ~ 'True, IsSubClassOf tgt 'E99_ ~ 'True)
         => Property_ 'E12_ 'E99_ src tgt
  -- | P186i_is_produced_by
  P186i :: (IsSubClassOf src 'E99_ ~ 'True, IsSubClassOf tgt 'E12_ ~ 'True)
         => Property_ 'E99_ 'E12_ src tgt
  -- | P187_has_production_plan
  P187  :: (IsSubClassOf src 'E99_ ~ 'True, IsSubClassOf tgt 'E29_ ~ 'True)
         => Property_ 'E99_ 'E29_ src tgt
  -- | P187i_is_production_plan_for
  P187i :: (IsSubClassOf src 'E29_ ~ 'True, IsSubClassOf tgt 'E99_ ~ 'True)
         => Property_ 'E29_ 'E99_ src tgt
  -- | P188_requires_production_tool
  P188  :: (IsSubClassOf src 'E99_ ~ 'True, IsSubClassOf tgt 'E19_ ~ 'True)
         => Property_ 'E99_ 'E19_ src tgt
  -- | P188i_is_production_tool_for
  P188i :: (IsSubClassOf src 'E19_ ~ 'True, IsSubClassOf tgt 'E99_ ~ 'True)
         => Property_ 'E19_ 'E99_ src tgt
  -- | P189_approximates
  P189  :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E53_ 'E53_ src tgt
  -- | P189i_is_approximated_by
  P189i :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E53_ 'E53_ src tgt
  -- | P190_has_symbolic_content
  P190  :: (IsSubClassOf src 'E90_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E90_ 'Literal_ src tgt
  -- | P191_had_duration
  P191  :: (IsSubClassOf src 'E52_ ~ 'True, IsSubClassOf tgt 'E54_ ~ 'True)
         => Property_ 'E52_ 'E54_ src tgt
  -- | P191i_was_duration_of
  P191i :: (IsSubClassOf src 'E54_ ~ 'True, IsSubClassOf tgt 'E52_ ~ 'True)
         => Property_ 'E54_ 'E52_ src tgt
  -- | P195_was_a_presence_of
  P195  :: (IsSubClassOf src 'E93_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E93_ 'E18_ src tgt
  -- | P195i_had_presence
  P195i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E93_ ~ 'True)
         => Property_ 'E18_ 'E93_ src tgt
  -- | P196_defines
  P196  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E92_ ~ 'True)
         => Property_ 'E18_ 'E92_ src tgt
  -- | P196i_is_defined_by
  P196i :: (IsSubClassOf src 'E92_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E92_ 'E18_ src tgt
  -- | P197_covered_parts_of
  P197  :: (IsSubClassOf src 'E93_ ~ 'True, IsSubClassOf tgt 'E53_ ~ 'True)
         => Property_ 'E93_ 'E53_ src tgt
  -- | P197i_was_partially_covered_by
  P197i :: (IsSubClassOf src 'E53_ ~ 'True, IsSubClassOf tgt 'E93_ ~ 'True)
         => Property_ 'E53_ 'E93_ src tgt
  -- | P198_holds_or_supports
  P198  :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E18_ 'E18_ src tgt
  -- | P198i_is_held_or_supported_by
  P198i :: (IsSubClassOf src 'E18_ ~ 'True, IsSubClassOf tgt 'E18_ ~ 'True)
         => Property_ 'E18_ 'E18_ src tgt
  -- | P01i_is_domain_of (Custom)
  P01i  :: (IsSubClassOf src 'E12_ ~ 'True, IsSubClassOf tgt 'PC14_ ~ 'True)
         => Property_ 'E12_ 'PC14_ src tgt
  -- | P02_has_range (Custom)
  P02   :: (IsSubClassOf src 'PC14_ ~ 'True, IsSubClassOf tgt 'E39_ ~ 'True)
         => Property_ 'PC14_ 'E39_ src tgt
  -- | custom:sameAs (our custom same as)
  SameAs :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E1_ 'E1_ src tgt
  -- | custom:objectSameAs (our custom same as)
  ObjectSameAs :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'E1_ ~ 'True)
         => Property_ 'E1_ 'E1_ src tgt

  -- | ImageStorageId
  ImageStorageId  :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E1_ 'Literal_ src tgt

  -- | custom:imageOrder
  ImageOrder :: (IsSubClassOf src 'E1_ ~ 'True, IsSubClassOf tgt 'Literal_ ~ 'True)
         => Property_ 'E1_ 'Literal_ src tgt


-- | Convert a CRM class into its canonical URI representation
classUri :: Class_ -> Text
classUri E1_  = "crm:E1_CRM_Entity"
classUri E2_  = "crm:E2_Temporal_Entity"
classUri E3_  = "crm:E3_Condition_State"
classUri E4_  = "crm:E4_Period"
classUri E5_  = "crm:E5_Event"
classUri E6_  = "crm:E6_Destruction"
classUri E7_  = "crm:E7_Activity"
classUri E8_  = "crm:E8_Acquisition"
classUri E9_  = "crm:E9_Move"
classUri E10_ = "crm:E10_Transfer_of_Custody"
classUri E11_ = "crm:E11_Modification"
classUri E12_ = "crm:E12_Production"
classUri E13_ = "crm:E13_Attribute_Assignment"
classUri E14_ = "crm:E14_Condition_Assessment"
classUri E15_ = "crm:E15_Identifier_Assignment"
classUri E16_ = "crm:E16_Measurement"
classUri E17_ = "crm:E17_Type_Assignment"
classUri E18_ = "crm:E18_Physical_Thing"
classUri E19_ = "crm:E19_Physical_Object"
classUri E20_ = "crm:E20_Biological_Object"
classUri E21_ = "crm:E21_Person"
classUri E22_ = "crm:E22_Human-Made_Object"
classUri E24_ = "crm:E24_Physical_Human-Made_Thing"
classUri E25_ = "crm:E25_Human-Made_Feature"
classUri E26_ = "crm:E26_Physical_Feature"
classUri E27_ = "crm:E27_Site"
classUri E28_ = "crm:E28_Conceptual_Object"
classUri E29_ = "crm:E29_Design_or_Procedure"
classUri E30_ = "crm:E30_Right"
classUri E31_ = "crm:E31_Document"
classUri E32_ = "crm:E32_Authority_Document"
classUri E33_ = "crm:E33_Linguistic_Object"
classUri E33_E41_ = "crm:E33_E41_Linguistic_Appellation"
classUri E34_ = "crm:E34_Inscription"
classUri E35_ = "crm:E35_Title"
classUri E36_ = "crm:E36_Visual_Item"
classUri E37_ = "crm:E37_Mark"
classUri E39_ = "crm:E39_Actor"
classUri E41_ = "crm:E41_Appellation"
classUri E42_ = "crm:E42_Identifier"
classUri E52_ = "crm:E52_Time-Span"
classUri E53_ = "crm:E53_Place"
classUri E54_ = "crm:E54_Dimension"
classUri E55_ = "crm:E55_Type"
classUri E56_ = "crm:E56_Language"
classUri E57_ = "crm:E57_Material"
classUri E58_ = "crm:E58_Measurement_Unit"
classUri E63_ = "crm:E63_Beginning_of_Existence"
classUri E64_ = "crm:E64_End_of_Existence"
classUri E65_ = "crm:E65_Creation"
classUri E66_ = "crm:E66_Formation"
classUri E67_ = "crm:E67_Birth"
classUri E68_ = "crm:E68_Dissolution"
classUri E69_ = "crm:E69_Death"
classUri E70_ = "crm:E70_Thing"
classUri E71_ = "crm:E71_Human-Made_Thing"
classUri E72_ = "crm:E72_Legal_Object"
classUri E73_ = "crm:E73_Information_Object"
classUri E74_ = "crm:E74_Group"
classUri E77_ = "crm:E77_Persistent_Item"
classUri E78_ = "crm:E78_Curated_Holding"
classUri E79_ = "crm:E79_Part_Addition"
classUri E80_ = "crm:E80_Part_Removal"
classUri E81_ = "crm:E81_Transformation"
classUri E83_ = "crm:E83_Type_Creation"
classUri E85_ = "crm:E85_Joining"
classUri E86_ = "crm:E86_Leaving"
classUri E87_ = "crm:E87_Curation_Activity"
classUri E89_ = "crm:E89_Propositional_Object"
classUri E90_ = "crm:E90_Symbolic_Object"
classUri E92_ = "crm:E92_Spacetime_Volume"
classUri E93_ = "crm:E93_Presence"
classUri E96_ = "crm:E96_Purchase"
classUri E97_ = "crm:E97_Monetary_Amount"
classUri E98_ = "crm:E98_Currency"
classUri E99_ = "crm:E99_Product_Type"
classUri PC14_ = "crm:PC14_carried_out_by" -- Assuming keep custom
classUri D1_ = "crmdig:D1_Digital_Object"
classUri Literal_ = "rdfs:Literal"

-- | Convert a CRM property into its canonical URI representation
propUri :: Property_ domain range from to -> Text
propUri P0    = "crm:P0"
propUri P1    = "crm:P1_is_identified_by"
propUri P1i   = "crm:P1i_identifies"
propUri P2    = "crm:P2_has_type"
propUri P2i   = "crm:P2i_is_type_of"
propUri P3    = "crm:P3_has_note"
propUri P4    = "crm:P4_has_time-span"
propUri P4i   = "crm:P4i_is_time-span_of"
propUri P5    = "crm:P5_consists_of"
propUri P5i   = "crm:P5i_forms_part_of"
propUri P7    = "crm:P7_took_place_at"
propUri P7i   = "crm:P7i_witnessed"
propUri P8    = "crm:P8_took_place_on_or_within"
propUri P8i   = "crm:P8i_witnessed"
propUri P9    = "crm:P9_consists_of"
propUri P9i   = "crm:P9i_forms_part_of"
propUri P10   = "crm:P10_falls_within"
propUri P10i  = "crm:P10i_contains"
propUri P11   = "crm:P11_had_participant"
propUri P11i  = "crm:P11i_participated_in"
propUri P12   = "crm:P12_occurred_in_the_presence_of"
propUri P12i  = "crm:P12i_was_present_at"
propUri P13   = "crm:P13_destroyed"
propUri P13i  = "crm:P13i_was_destroyed_by"
propUri P14   = "crm:P14_carried_out_by"
propUri P14i  = "crm:P14i_performed"
propUri P15   = "crm:P15_was_influenced_by"
propUri P15i  = "crm:P15i_influenced"
propUri P16   = "crm:P16_used_specific_object"
propUri P16i  = "crm:P16i_was_used_for"
propUri P17   = "crm:P17_was_motivated_by"
propUri P17i  = "crm:P17i_motivated"
propUri P19   = "crm:P19_was_intended_use_of"
propUri P19i  = "crm:P19i_was_made_for"
propUri P20   = "crm:P20_had_specific_purpose"
propUri P20i  = "crm:P20i_was_purpose_of"
propUri P21   = "crm:P21_had_general_purpose"
propUri P21i  = "crm:P21i_was_purpose_of"
propUri P22   = "crm:P22_transferred_title_to"
propUri P22i  = "crm:P22i_acquired_title_through"
propUri P23   = "crm:P23_transferred_title_from"
propUri P23i  = "crm:P23i_surrendered_title_through"
propUri P24   = "crm:P24_transferred_title_of"
propUri P24i  = "crm:P24i_changed_ownership_through"
propUri P25   = "crm:P25_moved"
propUri P25i  = "crm:P25i_moved_by"
propUri P26   = "crm:P26_moved_to"
propUri P26i  = "crm:P26i_was_destination_of"
propUri P27   = "crm:P27_moved_from"
propUri P27i  = "crm:P27i_was_origin_of"
propUri P28   = "crm:P28_custody_surrendered_by"
propUri P28i  = "crm:P28i_surrendered_custody_through"
propUri P29   = "crm:P29_custody_received_by"
propUri P29i  = "crm:P29i_received_custody_through"
propUri P30   = "crm:P30_transferred_custody_of"
propUri P30i  = "crm:P30i_custody_transferred_through"
propUri P31   = "crm:P31_has_modified"
propUri P31i  = "crm:P31i_was_modified_by"
propUri P32   = "crm:P32_used_general_technique"
propUri P32i  = "crm:P32i_was_technique_of"
propUri P33   = "crm:P33_used_specific_technique"
propUri P33i  = "crm:P33i_was_used_by"
propUri P34   = "crm:P34_concerned"
propUri P34i  = "crm:P34i_was_assessed_by"
propUri P35   = "crm:P35_has_identified"
propUri P35i  = "crm:P35i_was_identified_by"
propUri P37   = "crm:P37_assigned"
propUri P37i  = "crm:P37i_was_assigned_by"
propUri P38   = "crm:P38_deassigned"
propUri P38i  = "crm:P38i_was_deassigned_by"
propUri P39   = "crm:P39_measured"
propUri P39i  = "crm:P39i_was_measured_by"
propUri P40   = "crm:P40_observed_dimension"
propUri P40i  = "crm:P40i_was_observed_in"
propUri P41   = "crm:P41_classified"
propUri P41i  = "crm:P41i_was_classified_by"
propUri P42   = "crm:P42_assigned"
propUri P42i  = "crm:P42i_was_assigned_by"
propUri P43   = "crm:P43_has_dimension"
propUri P43i  = "crm:P43i_is_dimension_of"
propUri P44   = "crm:P44_has_condition"
propUri P44i  = "crm:P44i_is_condition_of"
propUri P45   = "crm:P45_consists_of"
propUri P45i  = "crm:P45i_is_incorporated_in"
propUri P46   = "crm:P46_is_composed_of"
propUri P46i  = "crm:P46i_forms_part_of"
propUri P48   = "crm:P48_has_preferred_identifier"
propUri P48i  = "crm:P48i_is_preferred_identifier_of"
propUri P49   = "crm:P49_has_former_or_current_keeper"
propUri P49i  = "crm:P49i_is_former_or_current_keeper_of"
propUri P50   = "crm:P50_has_current_keeper"
propUri P50i  = "crm:P50i_is_current_keeper_of"
propUri P51   = "crm:P51_has_former_or_current_owner"
propUri P51i  = "crm:P51i_is_former_or_current_owner_of"
propUri P52   = "crm:P52_has_current_owner"
propUri P52i  = "crm:P52i_is_current_owner_of"
propUri P53   = "crm:P53_has_former_or_current_location"
propUri P53i  = "crm:P53i_is_former_or_current_location_of"
propUri P54   = "crm:P54_has_current_permanent_location"
propUri P54i  = "crm:P54i_is_current_permanent_location_of"
propUri P55   = "crm:P55_has_current_location"
propUri P55i  = "crm:P55i_currently_holds"
propUri P56   = "crm:P56_bears_feature"
propUri P56i  = "crm:P56i_is_found_on"
propUri P57   = "crm:P57_has_number_of_parts"
propUri P59   = "crm:P59_has_section"
propUri P59i  = "crm:P59i_is_located_on_or_within"
propUri P62   = "crm:P62_depicts"
propUri P62i  = "crm:P62i_is_depicted_by"
propUri P65   = "crm:P65_shows_visual_item"
propUri P65i  = "crm:P65i_is_shown_by"
propUri P67   = "crm:P67_refers_to"
propUri P67i  = "crm:P67i_is_referred_to_by"
propUri P68   = "crm:P68_foresees_use_of"
propUri P68i  = "crm:P68i_use_foreseen_by"
propUri P69   = "crm:P69_has_association_with"
propUri P69i  = "crm:P69i_is_associated_with"
propUri P70   = "crm:P70_documents"
propUri P70i  = "crm:P70i_is_documented_in"
propUri P71   = "crm:P71_lists"
propUri P71i  = "crm:P71i_is_listed_in"
propUri P72   = "crm:P72_has_language"
propUri P72i  = "crm:P72i_is_language_of"
propUri P73   = "crm:P73_has_translation"
propUri P73i  = "crm:P73i_is_translation_of"
propUri P74   = "crm:P74_has_current_or_former_residence"
propUri P74i  = "crm:P74i_is_current_or_former_residence_of"
propUri P75   = "crm:P75_possesses"
propUri P75i  = "crm:P75i_is_possessed_by"
propUri P76   = "crm:P76_has_contact_point"
propUri P76i  = "crm:P76i_provides_access_to"
propUri P79   = "crm:P79_beginning_is_qualified_by"
propUri P80   = "crm:P80_end_is_qualified_by"
propUri P81   = "crm:P81_ongoing_throughout"
propUri P81a  = "crm:P81a_end_of_the_begin"
propUri P81b  = "crm:P81b_begin_of_the_end"
propUri P82   = "crm:P82_at_some_time_within"
propUri P82a  = "crm:P82a_begin_of_the_begin"
propUri P82b  = "crm:P82b_end_of_the_end"
propUri P86   = "crm:P86_falls_within"
propUri P86i  = "crm:P86i_contains"
propUri P89   = "crm:P89_falls_within"
propUri P89i  = "crm:P89i_contains"
propUri P90   = "crm:P90_has_value"
propUri P90a  = "crm:P90a_has_lower_value_limit"
propUri P90b  = "crm:P90b_has_upper_value_limit"
propUri P91   = "crm:P91_has_unit"
propUri P91i  = "crm:P91i_is_unit_of"
propUri P92   = "crm:P92_brought_into_existence"
propUri P92i  = "crm:P92i_was_brought_into_existence_by"
propUri P93   = "crm:P93_took_out_of_existence"
propUri P93i  = "crm:P93i_was_taken_out_of_existence_by"
propUri P94   = "crm:P94_has_created"
propUri P94i  = "crm:P94i_was_created_by"
propUri P95   = "crm:P95_has_formed"
propUri P95i  = "crm:P95i_was_formed_by"
propUri P96   = "crm:P96_by_mother"
propUri P96i  = "crm:P96i_gave_birth"
propUri P97   = "crm:P97_from_father"
propUri P97i  = "crm:P97i_was_father_for"
propUri P98   = "crm:P98_brought_into_life"
propUri P98i  = "crm:P98i_was_born"
propUri P99   = "crm:P99_dissolved"
propUri P99i  = "crm:P99i_was_dissolved_by"
propUri P100  = "crm:P100_was_death_of"
propUri P100i = "crm:P100i_died_in"
propUri P101  = "crm:P101_had_as_general_use"
propUri P101i = "crm:P101i_was_use_of"
propUri P102  = "crm:P102_has_title"
propUri P102i = "crm:P102i_is_title_of"
propUri P103  = "crm:P103_was_intended_for"
propUri P103i = "crm:P103i_was_intention_of"
propUri P104  = "crm:P104_is_subject_to"
propUri P104i = "crm:P104i_applies_to"
propUri P105  = "crm:P105_right_held_by"
propUri P105i = "crm:P105i_has_right_on"
propUri P106  = "crm:P106_is_composed_of"
propUri P106i = "crm:P106i_forms_part_of"
propUri P107  = "crm:P107_has_current_or_former_member"
propUri P107i = "crm:P107i_is_current_or_former_member_of"
propUri P108  = "crm:P108_has_produced"
propUri P108i = "crm:P108i_was_produced_by"
propUri P109  = "crm:P109_has_current_or_former_curator"
propUri P109i = "crm:P109i_is_current_or_former_curator_of"
propUri P110  = "crm:P110_augmented"
propUri P110i = "crm:P110i_was_augmented_by"
propUri P111  = "crm:P111_added"
propUri P111i = "crm:P111i_was_added_by"
propUri P112  = "crm:P112_diminished"
propUri P112i = "crm:P112i_was_diminished_by"
propUri P113  = "crm:P113_removed"
propUri P113i = "crm:P113i_was_removed_by"
propUri P121  = "crm:P121_overlaps_with"
propUri P122  = "crm:P122_borders_with"
propUri P123  = "crm:P123_resulted_in"
propUri P123i = "crm:P123i_resulted_from"
propUri P124  = "crm:P124_transformed"
propUri P124i = "crm:P124i_was_transformed_by"
propUri P125  = "crm:P125_used_object_of_type"
propUri P125i = "crm:P125i_was_type_of_object_used_in"
propUri P126  = "crm:P126_employed"
propUri P126i = "crm:P126i_was_employed_in"
propUri P127  = "crm:P127_has_broader_term"
propUri P127i = "crm:P127i_has_narrower_term"
propUri P128  = "crm:P128_carries"
propUri P128i = "crm:P128i_is_carried_by"
propUri P129  = "crm:P129_is_about"
propUri P129i = "crm:P129i_is_subject_of"
propUri P130  = "crm:P130_shows_features_of"
propUri P130i = "crm:P130i_features_are_also_found_on"
propUri P132  = "crm:P132_spatiotemporally_overlaps_with"
propUri P133  = "crm:P133_is_spatiotemporally_separated_from"
propUri P134  = "crm:P134_continued"
propUri P134i = "crm:P134i_was_continued_by"
propUri P135  = "crm:P135_created_type"
propUri P135i = "crm:P135i_was_created_by"
propUri P136  = "crm:P136_was_based_on"
propUri P136i = "crm:P136i_supported_type_creation"
propUri P137  = "crm:P137_exemplifies"
propUri P137i = "crm:P137i_is_exemplified_by"
propUri P138  = "crm:P138_represents"
propUri P138i = "crm:P138i_has_representation"
propUri P139  = "crm:P139_has_alternative_form"
propUri P139i = "crm:P139i_is_alternative_form_of"
propUri P140  = "crm:P140_assigned_attribute_to"
propUri P140i = "crm:P140i_was_attributed_by"
propUri P141  = "crm:P141_assigned"
propUri P141i = "crm:P141i_was_assigned_by"
propUri P142  = "crm:P142_used_constituent"
propUri P142i = "crm:P142i_was_used_in"
propUri P143  = "crm:P143_joined"
propUri P143i = "crm:P143i_was_joined_by"
propUri P144  = "crm:P144_joined_with"
propUri P144i = "crm:P144i_gained_member_by"
propUri P145  = "crm:P145_separated"
propUri P145i = "crm:P145i_left_by"
propUri P146  = "crm:P146_separated_from"
propUri P146i = "crm:P146i_lost_member_by"
propUri P147  = "crm:P147_curated"
propUri P147i = "crm:P147i_was_curated_by"
propUri P148  = "crm:P148_has_component"
propUri P148i = "crm:P148i_is_component_of"
propUri P150  = "crm:P150_defines_typical_parts_of"
propUri P150i = "crm:P150i_defines_typical_wholes_for"
propUri P151  = "crm:P151_was_formed_from"
propUri P151i = "crm:P151i_participated_in"
propUri P152  = "crm:P152_has_parent"
propUri P152i = "crm:P152i_is_parent_of"
propUri P156  = "crm:P156_occupies"
propUri P156i = "crm:P156i_is_occupied_by"
propUri P157  = "crm:P157_is_at_rest_relative_to"
propUri P157i = "crm:P157i_provides_reference_space_for"
propUri P160  = "crm:P160_has_temporal_projection"
propUri P160i = "crm:P160i_is_temporal_projection_of"
propUri P161  = "crm:P161_has_spatial_projection"
propUri P161i = "crm:P161i_is_spatial_projection_of"
propUri P164  = "crm:P164_is_temporally_specified_by"
propUri P164i = "crm:P164i_temporally_specifies"
propUri P165  = "crm:P165_incorporates"
propUri P165i = "crm:P165i_is_incorporated_in"
propUri P166  = "crm:P166_was_a_presence_of"
propUri P166i = "crm:P166i_had_presence"
propUri P167  = "crm:P167_was_within"
propUri P167i = "crm:P167i_includes"
propUri P168  = "crm:P168_place_is_defined_by"
propUri P169i = "crm:P169i_spacetime_volume_is_defined_by"
propUri P170i = "crm:P170i_time_is_defined_by"
propUri P171  = "crm:P171_at_some_place_within"
propUri P172  = "crm:P172_contains"
propUri P173  = "crm:P173_starts_before_or_with_the_end_of"
propUri P173i = "crm:P173i_ends_after_or_with_the_start_of"
propUri P174  = "crm:P174_starts_before_the_end_of"
propUri P174i = "crm:P174i_ends_after_the_start_of"
propUri P175  = "crm:P175_starts_before_or_with_the_start_of"
propUri P175i = "crm:P175i_starts_after_or_with_the_start_of"
propUri P176  = "crm:P176_starts_before_the_start_of"
propUri P176i = "crm:P176i_starts_after_the_start_of"
propUri P177  = "crm:P177_assigned_property_of_type"
propUri P177i = "crm:P177i_is_type_of_property_assigned"
propUri P179  = "crm:P179_had_sales_price"
propUri P179i = "crm:P179i_was_sales_price_of"
propUri P180  = "crm:P180_has_currency"
propUri P180i = "crm:P180i_was_currency_of"
propUri P182  = "crm:P182_ends_before_or_with_the_start_of"
propUri P182i = "crm:P182i_starts_after_or_with_the_end_of"
propUri P183  = "crm:P183_ends_before_the_start_of"
propUri P183i = "crm:P183i_starts_after_the_end_of"
propUri P184  = "crm:P184_ends_before_or_with_the_end_of"
propUri P184i = "crm:P184i_ends_with_or_after_the_end_of"
propUri P185  = "crm:P185_ends_before_the_end_of"
propUri P185i = "crm:P185i_ends_after_the_end_of"
propUri P186  = "crm:P186_produced_thing_of_product_type"
propUri P186i = "crm:P186i_is_produced_by"
propUri P187  = "crm:P187_has_production_plan"
propUri P187i = "crm:P187i_is_production_plan_for"
propUri P188  = "crm:P188_requires_production_tool"
propUri P188i = "crm:P188i_is_production_tool_for"
propUri P189  = "crm:P189_approximates"
propUri P189i = "crm:P189i_is_approximated_by"
propUri P190  = "crm:P190_has_symbolic_content"
propUri P191  = "crm:P191_had_duration"
propUri P191i = "crm:P191i_was_duration_of"
propUri P195  = "crm:P195_was_a_presence_of"
propUri P195i = "crm:P195i_had_presence"
propUri P196  = "crm:P196_defines"
propUri P196i = "crm:P196i_is_defined_by"
propUri P197  = "crm:P197_covered_parts_of"
propUri P197i = "crm:P197i_was_partially_covered_by"
propUri P198  = "crm:P198_holds_or_supports"
propUri P198i = "crm:P198i_is_held_or_supported_by"
propUri P01i  = "crm:P01i_is_domain_of" -- Assuming keep custom
propUri P02   = "crm:P02_has_range" -- Assuming keep custom
propUri SameAs = "custom:sameAs"
propUri ImageOrder = "custom:imageOrder" 
propUri ObjectSameAs = "custom:objectSameAs"
propUri ImageStorageId = "image-api:storage-id"

-- | Get the inverse of a property, if defined.
-- Returns Nothing if the property has no inverse (e.g., range is Literal).
getInverseOfProperty :: Property_ domain range from to -> Maybe (Property_ range domain to from)
getInverseOfProperty P0    = Nothing
getInverseOfProperty P1    = Just P1i
getInverseOfProperty P1i   = Just P1
getInverseOfProperty P2    = Just P2i
getInverseOfProperty P2i   = Just P2
getInverseOfProperty P4    = Just P4i
getInverseOfProperty P4i   = Just P4
getInverseOfProperty P5    = Just P5i
getInverseOfProperty P5i   = Just P5
getInverseOfProperty P7    = Just P7i
getInverseOfProperty P7i   = Just P7
getInverseOfProperty P8    = Just P8i
getInverseOfProperty P8i   = Just P8
getInverseOfProperty P9    = Just P9i
getInverseOfProperty P9i   = Just P9
getInverseOfProperty P10   = Just P10i
getInverseOfProperty P10i  = Just P10
getInverseOfProperty P11   = Just P11i
getInverseOfProperty P11i  = Just P11
getInverseOfProperty P12   = Just P12i
getInverseOfProperty P12i  = Just P12
getInverseOfProperty P13   = Just P13i
getInverseOfProperty P13i  = Just P13
getInverseOfProperty P14   = Just P14i
getInverseOfProperty P14i  = Just P14
getInverseOfProperty P15   = Just P15i
getInverseOfProperty P15i  = Just P15
getInverseOfProperty P16   = Just P16i
getInverseOfProperty P16i  = Just P16
getInverseOfProperty P17   = Just P17i
getInverseOfProperty P17i  = Just P17
getInverseOfProperty P19   = Just P19i
getInverseOfProperty P19i  = Just P19
getInverseOfProperty P20   = Just P20i
getInverseOfProperty P20i  = Just P20
getInverseOfProperty P21   = Just P21i
getInverseOfProperty P21i  = Just P21
getInverseOfProperty P22   = Just P22i
getInverseOfProperty P22i  = Just P22
getInverseOfProperty P23   = Just P23i
getInverseOfProperty P23i  = Just P23
getInverseOfProperty P24   = Just P24i
getInverseOfProperty P24i  = Just P24
getInverseOfProperty P25   = Just P25i
getInverseOfProperty P25i  = Just P25
getInverseOfProperty P26   = Just P26i
getInverseOfProperty P26i  = Just P26
getInverseOfProperty P27   = Just P27i
getInverseOfProperty P27i  = Just P27
getInverseOfProperty P28   = Just P28i
getInverseOfProperty P28i  = Just P28
getInverseOfProperty P29   = Just P29i
getInverseOfProperty P29i  = Just P29
getInverseOfProperty P30   = Just P30i
getInverseOfProperty P30i  = Just P30
getInverseOfProperty P31   = Just P31i
getInverseOfProperty P31i  = Just P31
getInverseOfProperty P32   = Just P32i
getInverseOfProperty P32i  = Just P32
getInverseOfProperty P33   = Just P33i
getInverseOfProperty P33i  = Just P33
getInverseOfProperty P34   = Just P34i
getInverseOfProperty P34i  = Just P34
getInverseOfProperty P35   = Just P35i
getInverseOfProperty P35i  = Just P35
getInverseOfProperty P37   = Just P37i
getInverseOfProperty P37i  = Just P37
getInverseOfProperty P38   = Just P38i
getInverseOfProperty P38i  = Just P38
getInverseOfProperty P39   = Just P39i
getInverseOfProperty P39i  = Just P39
getInverseOfProperty P40   = Just P40i
getInverseOfProperty P40i  = Just P40
getInverseOfProperty P41   = Just P41i
getInverseOfProperty P41i  = Just P41
getInverseOfProperty P42   = Just P42i
getInverseOfProperty P42i  = Just P42
getInverseOfProperty P43   = Just P43i
getInverseOfProperty P43i  = Just P43
getInverseOfProperty P44   = Just P44i
getInverseOfProperty P44i  = Just P44
getInverseOfProperty P45   = Just P45i
getInverseOfProperty P45i  = Just P45
getInverseOfProperty P46   = Just P46i
getInverseOfProperty P46i  = Just P46
getInverseOfProperty P48   = Just P48i
getInverseOfProperty P48i  = Just P48
getInverseOfProperty P49   = Just P49i
getInverseOfProperty P49i  = Just P49
getInverseOfProperty P50   = Just P50i
getInverseOfProperty P50i  = Just P50
getInverseOfProperty P51   = Just P51i
getInverseOfProperty P51i  = Just P51
getInverseOfProperty P52   = Just P52i
getInverseOfProperty P52i  = Just P52
getInverseOfProperty P53   = Just P53i
getInverseOfProperty P53i  = Just P53
getInverseOfProperty P54   = Just P54i
getInverseOfProperty P54i  = Just P54
getInverseOfProperty P55   = Just P55i
getInverseOfProperty P55i  = Just P55
getInverseOfProperty P56   = Just P56i
getInverseOfProperty P56i  = Just P56
getInverseOfProperty P59   = Just P59i
getInverseOfProperty P59i  = Just P59
getInverseOfProperty P62   = Just P62i
getInverseOfProperty P62i  = Just P62
getInverseOfProperty P65   = Just P65i
getInverseOfProperty P65i  = Just P65
getInverseOfProperty P67   = Just P67i
getInverseOfProperty P67i  = Just P67
getInverseOfProperty P68   = Just P68i
getInverseOfProperty P68i  = Just P68
getInverseOfProperty P69   = Just P69i
getInverseOfProperty P69i  = Just P69
getInverseOfProperty P70   = Just P70i
getInverseOfProperty P70i  = Just P70
getInverseOfProperty P71   = Just P71i
getInverseOfProperty P71i  = Just P71
getInverseOfProperty P72   = Just P72i
getInverseOfProperty P72i  = Just P72
getInverseOfProperty P73   = Just P73i
getInverseOfProperty P73i  = Just P73
getInverseOfProperty P74   = Just P74i
getInverseOfProperty P74i  = Just P74
getInverseOfProperty P75   = Just P75i
getInverseOfProperty P75i  = Just P75
getInverseOfProperty P76   = Just P76i
getInverseOfProperty P76i  = Just P76
getInverseOfProperty P86   = Just P86i
getInverseOfProperty P86i  = Just P86
getInverseOfProperty P89   = Just P89i
getInverseOfProperty P89i  = Just P89
getInverseOfProperty P91   = Just P91i
getInverseOfProperty P91i  = Just P91
getInverseOfProperty P92   = Just P92i
getInverseOfProperty P92i  = Just P92
getInverseOfProperty P93   = Just P93i
getInverseOfProperty P93i  = Just P93
getInverseOfProperty P94   = Just P94i
getInverseOfProperty P94i  = Just P94
getInverseOfProperty P95   = Just P95i
getInverseOfProperty P95i  = Just P95
getInverseOfProperty P96   = Just P96i
getInverseOfProperty P96i  = Just P96
getInverseOfProperty P97   = Just P97i
getInverseOfProperty P97i  = Just P97
getInverseOfProperty P98   = Just P98i
getInverseOfProperty P98i  = Just P98
getInverseOfProperty P99   = Just P99i
getInverseOfProperty P99i  = Just P99
getInverseOfProperty P100  = Just P100i
getInverseOfProperty P100i = Just P100
getInverseOfProperty P101  = Just P101i
getInverseOfProperty P101i = Just P101
getInverseOfProperty P102  = Just P102i
getInverseOfProperty P102i = Just P102
getInverseOfProperty P103  = Just P103i
getInverseOfProperty P103i = Just P103
getInverseOfProperty P104  = Just P104i
getInverseOfProperty P104i = Just P104
getInverseOfProperty P105  = Just P105i
getInverseOfProperty P105i = Just P105
getInverseOfProperty P106  = Just P106i
getInverseOfProperty P106i = Just P106
getInverseOfProperty P107  = Just P107i
getInverseOfProperty P107i = Just P107
getInverseOfProperty P108  = Just P108i
getInverseOfProperty P108i = Just P108
getInverseOfProperty P109  = Just P109i
getInverseOfProperty P109i = Just P109
getInverseOfProperty P110  = Just P110i
getInverseOfProperty P110i = Just P110
getInverseOfProperty P111  = Just P111i
getInverseOfProperty P111i = Just P111
getInverseOfProperty P112  = Just P112i
getInverseOfProperty P112i = Just P112
getInverseOfProperty P113  = Just P113i
getInverseOfProperty P113i = Just P113
getInverseOfProperty P123  = Just P123i
getInverseOfProperty P123i = Just P123
getInverseOfProperty P124  = Just P124i
getInverseOfProperty P124i = Just P124
getInverseOfProperty P125  = Just P125i
getInverseOfProperty P125i = Just P125
getInverseOfProperty P126  = Just P126i
getInverseOfProperty P126i = Just P126
getInverseOfProperty P127  = Just P127i
getInverseOfProperty P127i = Just P127
getInverseOfProperty P128  = Just P128i
getInverseOfProperty P128i = Just P128
getInverseOfProperty P129  = Just P129i
getInverseOfProperty P129i = Just P129
getInverseOfProperty P130  = Just P130i
getInverseOfProperty P130i = Just P130
getInverseOfProperty P134  = Just P134i
getInverseOfProperty P134i = Just P134
getInverseOfProperty P135  = Just P135i
getInverseOfProperty P135i = Just P135
getInverseOfProperty P136  = Just P136i
getInverseOfProperty P136i = Just P136
getInverseOfProperty P137  = Just P137i
getInverseOfProperty P137i = Just P137
getInverseOfProperty P138  = Just P138i
getInverseOfProperty P138i = Just P138
getInverseOfProperty P139  = Just P139i
getInverseOfProperty P139i = Just P139
getInverseOfProperty P140  = Just P140i
getInverseOfProperty P140i = Just P140
getInverseOfProperty P141  = Just P141i
getInverseOfProperty P141i = Just P141
getInverseOfProperty P142  = Just P142i
getInverseOfProperty P142i = Just P142
getInverseOfProperty P143  = Just P143i
getInverseOfProperty P143i = Just P143
getInverseOfProperty P144  = Just P144i
getInverseOfProperty P144i = Just P144
getInverseOfProperty P145  = Just P145i
getInverseOfProperty P145i = Just P145
getInverseOfProperty P146  = Just P146i
getInverseOfProperty P146i = Just P146
getInverseOfProperty P147  = Just P147i
getInverseOfProperty P147i = Just P147
getInverseOfProperty P148  = Just P148i
getInverseOfProperty P148i = Just P148
getInverseOfProperty P150  = Just P150i
getInverseOfProperty P150i = Just P150
getInverseOfProperty P151  = Just P151i
getInverseOfProperty P151i = Just P151
getInverseOfProperty P152  = Just P152i
getInverseOfProperty P152i = Just P152
getInverseOfProperty P156  = Just P156i
getInverseOfProperty P156i = Just P156
getInverseOfProperty P157  = Just P157i
getInverseOfProperty P157i = Just P157
getInverseOfProperty P160  = Just P160i
getInverseOfProperty P160i = Just P160
getInverseOfProperty P161  = Just P161i
getInverseOfProperty P161i = Just P161
getInverseOfProperty P164  = Just P164i
getInverseOfProperty P164i = Just P164
getInverseOfProperty P165  = Just P165i
getInverseOfProperty P165i = Just P165
getInverseOfProperty P166  = Just P166i
getInverseOfProperty P166i = Just P166
getInverseOfProperty P167  = Just P167i
getInverseOfProperty P167i = Just P167
getInverseOfProperty P173  = Just P173i
getInverseOfProperty P173i = Just P173
getInverseOfProperty P174  = Just P174i
getInverseOfProperty P174i = Just P174
getInverseOfProperty P175  = Just P175i
getInverseOfProperty P175i = Just P175
getInverseOfProperty P176  = Just P176i
getInverseOfProperty P176i = Just P176
getInverseOfProperty P177  = Just P177i
getInverseOfProperty P177i = Just P177
getInverseOfProperty P179  = Just P179i
getInverseOfProperty P179i = Just P179
getInverseOfProperty P180  = Just P180i
getInverseOfProperty P180i = Just P180
getInverseOfProperty P182  = Just P182i
getInverseOfProperty P182i = Just P182
getInverseOfProperty P183  = Just P183i
getInverseOfProperty P183i = Just P183
getInverseOfProperty P184  = Just P184i
getInverseOfProperty P184i = Just P184
getInverseOfProperty P185  = Just P185i
getInverseOfProperty P185i = Just P185
getInverseOfProperty P186  = Just P186i
getInverseOfProperty P186i = Just P186
getInverseOfProperty P187  = Just P187i
getInverseOfProperty P187i = Just P187
getInverseOfProperty P188  = Just P188i
getInverseOfProperty P188i = Just P188
getInverseOfProperty P189  = Just P189i
getInverseOfProperty P189i = Just P189
getInverseOfProperty P191  = Just P191i
getInverseOfProperty P191i = Just P191
getInverseOfProperty P195  = Just P195i
getInverseOfProperty P195i = Just P195
getInverseOfProperty P196  = Just P196i
getInverseOfProperty P196i = Just P196
getInverseOfProperty P197  = Just P197i
getInverseOfProperty P197i = Just P197
getInverseOfProperty P198  = Just P198i
getInverseOfProperty P198i = Just P198
getInverseOfProperty SameAs = Nothing -- we don't want to duplicate sameAs
getInverseOfProperty ObjectSameAs = Nothing -- we don't want to duplicate objectSameAs
-- Properties with Literal range have no inverse
getInverseOfProperty P3    = Nothing
getInverseOfProperty P57   = Nothing
getInverseOfProperty P79   = Nothing
getInverseOfProperty P80   = Nothing
getInverseOfProperty P81   = Nothing
getInverseOfProperty P81a  = Nothing
getInverseOfProperty P81b  = Nothing
getInverseOfProperty P82   = Nothing
getInverseOfProperty P82a  = Nothing
getInverseOfProperty P82b  = Nothing
getInverseOfProperty P90   = Nothing
getInverseOfProperty P90a  = Nothing
getInverseOfProperty P90b  = Nothing
getInverseOfProperty P168  = Nothing
getInverseOfProperty P169i = Nothing
getInverseOfProperty P170i = Nothing
getInverseOfProperty P171  = Nothing
getInverseOfProperty P172  = Nothing
getInverseOfProperty P190  = Nothing
-- Custom properties P01i and P02 do not have defined inverses in Property_ GADT
getInverseOfProperty P01i  = Nothing
getInverseOfProperty P02   = Nothing
-- Symmetric properties without explicit inverse definitions
getInverseOfProperty P121  = Just P121 -- symetric
getInverseOfProperty P122  = Just P122 -- symetric
getInverseOfProperty P132  = Just P132 -- symetric
getInverseOfProperty P133  = Just P133 -- symetric
getInverseOfProperty ImageStorageId = Nothing
getInverseOfProperty ImageOrder = Nothing

-- | E1_CRM_Entity
pattern E1 :: ClassRef 'E1_
pattern E1 = ClassRef "crm:E1_CRM_Entity"

-- | E2_Temporal_Entity
pattern E2 :: ClassRef 'E2_
pattern E2 = ClassRef "crm:E2_Temporal_Entity"

-- | E3_Condition_State
pattern E3 :: ClassRef 'E3_
pattern E3 = ClassRef "crm:E3_Condition_State"

-- | E4_Period
pattern E4 :: ClassRef 'E4_
pattern E4 = ClassRef "crm:E4_Period"

-- | E5_Event
pattern E5 :: ClassRef 'E5_
pattern E5 = ClassRef "crm:E5_Event"

-- | E6_Destruction
pattern E6 :: ClassRef 'E6_
pattern E6 = ClassRef "crm:E6_Destruction"

-- | E7_Activity
pattern E7 :: ClassRef 'E7_
pattern E7 = ClassRef "crm:E7_Activity"

-- | E55_Type
pattern E55 :: ClassRef 'E55_
pattern E55 = ClassRef "crm:E55_Type"

-- | E57_Material
pattern E57 :: ClassRef 'E57_
pattern E57 = ClassRef "crm:E57_Material"

-- | E74_Group
pattern E74 :: ClassRef 'E74_
pattern E74 = ClassRef "crm:E74_Group"

-- | E52_Time-Span
pattern E52 :: ClassRef 'E52_
pattern E52 = ClassRef "crm:E52_Time-Span"

-- | E53_Place
pattern E53 :: ClassRef 'E53_
pattern E53 = ClassRef "crm:E53_Place"

-- | E73_Information_Object
pattern E73 :: ClassRef 'E73_
pattern E73 = ClassRef "crm:E73_Information_Object"

-- | PC14_carried_out_by (Custom)
pattern PC14 :: ClassRef 'PC14_
pattern PC14 = ClassRef "crm:PC14_carried_out_by"

-- | E36_Visual_Item
pattern E36 :: ClassRef 'E36_
pattern E36 = ClassRef "crm:E36_Visual_Item"

-- | E8_Acquisition
pattern E8 :: ClassRef 'E8_
pattern E8 = ClassRef "crm:E8_Acquisition"

-- | E9_Move
pattern E9 :: ClassRef 'E9_
pattern E9 = ClassRef "crm:E9_Move"

-- | E10_Transfer_of_Custody
pattern E10 :: ClassRef 'E10_
pattern E10 = ClassRef "crm:E10_Transfer_of_Custody"

-- | E11_Modification
pattern E11 :: ClassRef 'E11_
pattern E11 = ClassRef "crm:E11_Modification"

-- | E12_Production
pattern E12 :: ClassRef 'E12_
pattern E12 = ClassRef "crm:E12_Production"

-- | E13_Attribute_Assignment
pattern E13 :: ClassRef 'E13_
pattern E13 = ClassRef "crm:E13_Attribute_Assignment"

-- | E14_Condition_Assessment
pattern E14 :: ClassRef 'E14_
pattern E14 = ClassRef "crm:E14_Condition_Assessment"

-- | E15_Identifier_Assignment
pattern E15 :: ClassRef 'E15_
pattern E15 = ClassRef "crm:E15_Identifier_Assignment"

-- | E16_Measurement
pattern E16 :: ClassRef 'E16_
pattern E16 = ClassRef "crm:E16_Measurement"

-- | E17_Type_Assignment
pattern E17 :: ClassRef 'E17_
pattern E17 = ClassRef "crm:E17_Type_Assignment"

-- | E18_Physical_Thing
pattern E18 :: ClassRef 'E18_
pattern E18 = ClassRef "crm:E18_Physical_Thing"

-- | E19_Physical_Object
pattern E19 :: ClassRef 'E19_
pattern E19 = ClassRef "crm:E19_Physical_Object"

-- | E20_Biological_Object
pattern E20 :: ClassRef 'E20_
pattern E20 = ClassRef "crm:E20_Biological_Object"

-- | E21_Person
pattern E21 :: ClassRef 'E21_
pattern E21 = ClassRef "crm:E21_Person"

-- | E22_Human-Made_Object
pattern E22 :: ClassRef 'E22_
pattern E22 = ClassRef "crm:E22_Human-Made_Object"

-- | E24_Physical_Human-Made_Thing
pattern E24 :: ClassRef 'E24_
pattern E24 = ClassRef "crm:E24_Physical_Human-Made_Thing"

-- | E25_Human-Made_Feature
pattern E25 :: ClassRef 'E25_
pattern E25 = ClassRef "crm:E25_Human-Made_Feature"

-- | E26_Physical_Feature
pattern E26 :: ClassRef 'E26_
pattern E26 = ClassRef "crm:E26_Physical_Feature"

-- | E27_Site
pattern E27 :: ClassRef 'E27_
pattern E27 = ClassRef "crm:E27_Site"

-- | E28_Conceptual_Object
pattern E28 :: ClassRef 'E28_
pattern E28 = ClassRef "crm:E28_Conceptual_Object"

-- | E29_Design_or_Procedure
pattern E29 :: ClassRef 'E29_
pattern E29 = ClassRef "crm:E29_Design_or_Procedure"

-- | E30_Right
pattern E30 :: ClassRef 'E30_
pattern E30 = ClassRef "crm:E30_Right"

-- | E31_Document
pattern E31 :: ClassRef 'E31_
pattern E31 = ClassRef "crm:E31_Document"

-- | E32_Authority_Document
pattern E32 :: ClassRef 'E32_
pattern E32 = ClassRef "crm:E32_Authority_Document"

-- | E33_Linguistic_Object
pattern E33 :: ClassRef 'E33_
pattern E33 = ClassRef "crm:E33_Linguistic_Object"

-- | E33_E41_Linguistic_Appellation
pattern E33_E41 :: ClassRef 'E33_E41_
pattern E33_E41 = ClassRef "crm:E33_E41_Linguistic_Appellation"

-- | E34_Inscription
pattern E34 :: ClassRef 'E34_
pattern E34 = ClassRef "crm:E34_Inscription"

-- | E35_Title
pattern E35 :: ClassRef 'E35_
pattern E35 = ClassRef "crm:E35_Title"

-- | E37_Mark
pattern E37 :: ClassRef 'E37_
pattern E37 = ClassRef "crm:E37_Mark"

-- | E39_Actor
pattern E39 :: ClassRef 'E39_
pattern E39 = ClassRef "crm:E39_Actor"

-- | E41_Appellation
pattern E41 :: ClassRef 'E41_
pattern E41 = ClassRef "crm:E41_Appellation"

-- | E42_Identifier
pattern E42 :: ClassRef 'E42_
pattern E42 = ClassRef "crm:E42_Identifier"

-- | E54_Dimension
pattern E54 :: ClassRef 'E54_
pattern E54 = ClassRef "crm:E54_Dimension"

-- | E56_Language
pattern E56 :: ClassRef 'E56_
pattern E56 = ClassRef "crm:E56_Language"

-- | E58_Measurement_Unit
pattern E58 :: ClassRef 'E58_
pattern E58 = ClassRef "crm:E58_Measurement_Unit"

-- | E63_Beginning_of_Existence
pattern E63 :: ClassRef 'E63_
pattern E63 = ClassRef "crm:E63_Beginning_of_Existence"

-- | E64_End_of_Existence
pattern E64 :: ClassRef 'E64_
pattern E64 = ClassRef "crm:E64_End_of_Existence"

-- | E65_Creation
pattern E65 :: ClassRef 'E65_
pattern E65 = ClassRef "crm:E65_Creation"

-- | E66_Formation
pattern E66 :: ClassRef 'E66_
pattern E66 = ClassRef "crm:E66_Formation"

-- | E67_Birth
pattern E67 :: ClassRef 'E67_
pattern E67 = ClassRef "crm:E67_Birth"

-- | E68_Dissolution
pattern E68 :: ClassRef 'E68_
pattern E68 = ClassRef "crm:E68_Dissolution"

-- | E69_Death
pattern E69 :: ClassRef 'E69_
pattern E69 = ClassRef "crm:E69_Death"

-- | E70_Thing
pattern E70 :: ClassRef 'E70_
pattern E70 = ClassRef "crm:E70_Thing"

-- | E71_Human-Made_Thing
pattern E71 :: ClassRef 'E71_
pattern E71 = ClassRef "crm:E71_Human-Made_Thing"

-- | E72_Legal_Object
pattern E72 :: ClassRef 'E72_
pattern E72 = ClassRef "crm:E72_Legal_Object"

-- | E77_Persistent_Item
pattern E77 :: ClassRef 'E77_
pattern E77 = ClassRef "crm:E77_Persistent_Item"

-- | E78_Curated_Holding
pattern E78 :: ClassRef 'E78_
pattern E78 = ClassRef "crm:E78_Curated_Holding"

-- | E79_Part_Addition
pattern E79 :: ClassRef 'E79_
pattern E79 = ClassRef "crm:E79_Part_Addition"

-- | E80_Part_Removal
pattern E80 :: ClassRef 'E80_
pattern E80 = ClassRef "crm:E80_Part_Removal"

-- | E81_Transformation
pattern E81 :: ClassRef 'E81_
pattern E81 = ClassRef "crm:E81_Transformation"

-- | E83_Type_Creation
pattern E83 :: ClassRef 'E83_
pattern E83 = ClassRef "crm:E83_Type_Creation"

-- | E85_Joining
pattern E85 :: ClassRef 'E85_
pattern E85 = ClassRef "crm:E85_Joining"

-- | E86_Leaving
pattern E86 :: ClassRef 'E86_
pattern E86 = ClassRef "crm:E86_Leaving"

-- | E87_Curation_Activity
pattern E87 :: ClassRef 'E87_
pattern E87 = ClassRef "crm:E87_Curation_Activity"

-- | E89_Propositional_Object
pattern E89 :: ClassRef 'E89_
pattern E89 = ClassRef "crm:E89_Propositional_Object"

-- | E90_Symbolic_Object
pattern E90 :: ClassRef 'E90_
pattern E90 = ClassRef "crm:E90_Symbolic_Object"

-- | E92_Spacetime_Volume
pattern E92 :: ClassRef 'E92_
pattern E92 = ClassRef "crm:E92_Spacetime_Volume"

-- | E93_Presence
pattern E93 :: ClassRef 'E93_
pattern E93 = ClassRef "crm:E93_Presence"

-- | E96_Purchase
pattern E96 :: ClassRef 'E96_
pattern E96 = ClassRef "crm:E96_Purchase"

-- | E97_Monetary_Amount
pattern E97 :: ClassRef 'E97_
pattern E97 = ClassRef "crm:E97_Monetary_Amount"

-- | E98_Currency
pattern E98 :: ClassRef 'E98_
pattern E98 = ClassRef "crm:E98_Currency"

-- | E99_Product_Type
pattern E99 :: ClassRef 'E99_
pattern E99 = ClassRef "crm:E99_Product_Type"

-- | D1_Digital_Object
pattern D1 :: ClassRef 'D1_
pattern D1 = ClassRef "crm:D1_Digital_Object"

-- | Literal
pattern LiteralType :: ClassRef 'Literal_
pattern LiteralType = ClassRef "Literal"
