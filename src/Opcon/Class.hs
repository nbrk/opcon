module Opcon.Class where

import Data.Hashable

-- | Neccessary info about the echelon level
class (Ord e, Show e) =>
  EchelonLevel e where


-- | Neccessary info about the unit
class (Eq a, Show a) => Organization a where

