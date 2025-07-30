module Mappings.Mappings.Work.Photo (photoLink) where

import CommonImports

-- |
-- Link from Artwork to Photo. The actuall photo is mapped in Photo.hs.
photoLink :: PathTree E22_
photoLink = 
  [x|ALLEGATI/FOTO|] @> (
    P138i ---> (E36, relativeUri "/visual_item") ==> [
      P65i ---> (E22, relativeUriT "/photo/{id}" [("id", [x|@sercdf|])]) ==> [

        -- @progr specifies preferred order of images for a given work.
        -- Francesca:
        --   In Zeri's XMLs, the correct order of the images associated with an artwork record is defined by the attribute “prog=1”, “prog=2.... 
        --   followed by the identifiers, where ”prog=1 is the image to be displayed by fist 
        --   (often corresponding to the entire or most recent photograph) and the details are at the bottom. 
        --   This attribute should be considered for not having a detail as a reference thumbnail in the result list.
        ImageOrder --> (typedLiteral [x|@progr|] (PrefixedURI "xsd:integer"))
      ]
    ]
  )
