{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Vocabularies.PHAROS where

import Vocabularies.Base

base = "pharos-meta:"

-- Work Types
built_work = base </> "built_work"
built_work_part = base </> "built_work_part"

-- Identifier types
identifier = base </> "identifier"
preferred_identifier = base </> "preferred_identifier"

attribution = base </> "attribution"

attributed_to = base </> "attributed_to"

-- Appellation types
preferred_name = base </> "preferred_name"
alternative_name = base </> "alternative_name"
pharos_preferred_name = base </> "pharos_preferred_name"

anonymous = base </> "anonymous_author"
anonymous_group = base </> "anonymous_author_group"

catalog_record = base </> "catalog_record"
catalog_url = base </> "catalog_url"

oai_pmh_url = base </> "oai_pmh_url"

catalog_record_update_event = base </> "catalog_record_update_event"

-- When a catalog record was updated last time
catalog_record_update_time = base </> "catalog_record_update_time"

-- When a catalog record was fetched from the source by Pharos
catalog_record_fetch_time = base </> "catalog_record_fetch_time"

-- Place Types
geographical_entity = base </> "geographical_entity"
country = base </> "country"
region = base </> "region"
city = base </> "city"
repository = base </> "repository"

-- Photo Types
photograph = base </> "photograph"
photograph_negative = base </> "photograph_negative"
photographic_print = base </> "photographic_print"
digital_image = base </> "digital_image"

photo_file_url = base </> "photo_file_url"

-- Licenses
license_type = base </> "license_type"

-- Genders
male = base </> "male"
female = base </> "female"

-- Nationality
nationality = base </> "nationality"

-- Roles
photographer = base </> "photographer"
photograph_umbrella_term = base </> "photograph_umbrella_term"

-- Subjects
subject = base </> "subject"
iconclass = base </> "iconclass"

-- Types
object_type = base </> "object_type"
material = base </> "material"