module Class where

import Data.Hashable

-- | Neccessary info about the echelon level
class (Eq e, Bounded e, Ord e, Enum e, Show e) =>
  EchelonLevel e where


-- | Neccessary info about the unit
class (Eq a, Show a) => Organization a where

