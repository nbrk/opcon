module Command where

import Types


-- | Echelon level of the command link
echelonOf :: Command e -> e
echelonOf (Organic e) = e
echelonOf (Opcon e) = e


-- | Is this an organic attachment
isOrganic :: Command e -> Bool
isOrganic (Organic _) = True
isOrganic _ = False
