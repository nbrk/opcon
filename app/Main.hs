module Main where

import Class
import Types
import Instances
import Hierarchy
import HierarchyNode

import Data.Hashable


-- | Example echelon levels
data Echelon
  = Team
  | Squad
  | Section
  | Platoon
  | Company
  | Battalion
  | Regiment
  | Brigade
  | Division
  | Corps
  | Army
  | ArmyGroup
  | Theater
  deriving (Eq, Ord, Enum, Bounded, Show)

instance EchelonLevel Echelon

-- data Unit
--   = Unit
--   { unitName :: String
--   }
--   deriving (Eq, Ord)
-- instance Show Unit where
--   show u = show (unitName u)


-- instance Organization Unit


genCoy :: String -> IO (Hierarchy String Echelon)
genCoy nam = do
  p1 <- mkHierarchy "1st" Platoon [] []
  p2 <- mkHierarchy "2nd" Platoon [] []
  p3 <- mkHierarchy "3rd" Platoon [] []
  mkHierarchy nam Company [p1, p2, p3] []

genBn :: String -> IO (Hierarchy String Echelon)
genBn nam = do
  c1 <- genCoy "A"
  c2 <- genCoy "B"
  c3 <- genCoy "C"
  p <- mkHierarchy "Rc Plt" Platoon [] []

  mkHierarchy nam Battalion [c1, c2, c3] [p]


genBde :: String -> IO (Hierarchy String Echelon)
genBde nam = do
  b1 <- genBn "1 Bn"
  b2 <- genBn "2 Bn"
  b3 <- genBn "3 Bn"
  c <- mkHierarchy "Arty coy" Company [] []

  mkHierarchy nam Brigade [b1, b2, b3] [c]


genDiv :: IO (Hierarchy String Echelon)
genDiv = do
  b1 <- genBde "5 IB"
  b2 <- genBde "3 AB"
  b3 <- genBde "4 AB"
  b <- mkHierarchy "Divarty bn" Battalion [] []

  mkHierarchy "XX Div" Division [b1, b2, b3, b] []


-- bn :: Hierarchy String Echelon
-- bn =
--   let mn x = mkHierarchyNodeWith (Just x)
--       mne = mkHierarchyNodeWith Nothing
--       mh = mkHierarchyWith
--       s1 = mh (mne 0) Squad (mn "Z Sq" 1) [] []
--       s2 = mh (mne 2) Squad (mn "X Sq" 3) [] []

--       p1 = mh (mne 4) Platoon (mn "1st" 5) [] []
--       p2 = mh (mne 6) Platoon (mn "2nd" 7) [] []
--       p3 = mh (mne 8) Platoon (mn "3rd" 9) [] []

--       c1 = mh (mne 10) Company (mn "A" 11) [p1, p2] []
--       c2 = mh (mne 12) Company (mn "B" 13) [p3] [s2]
--       c3 = mh (mne 14) Company (mn "C" 15) [] []
--   in
--     mh (mne 10) Battalion (mn "2 Bn" 11) [c1, c2, c3] [s1]


main :: IO ()
main = do

  putStrLn "Lala"
