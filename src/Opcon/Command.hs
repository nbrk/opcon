module Opcon.Command where

import Opcon.Types


-- | Echelon level of the command link
echelonLevel :: Command e -> e
echelonLevel (Organic e) = e
echelonLevel (Opcon e) = e


-- | Is this an organic attachment
isOrganic :: Command e -> Bool
isOrganic (Organic _) = True
isOrganic _ = False


-- | Change command nature type to organic
toOrganic :: Command e -> Command e
toOrganic (Organic e) = Organic e
toOrganic (Opcon e) = Opcon e


-- | Change command nature type to opcon
toOpcon :: Command e -> Command e
toOpcon (Organic e) = Opcon e
toOpcon (Opcon e) = Opcon e
