module Opcon.Example where

import Opcon.Types
import Opcon.Class
import Opcon.Hierarchy

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


-- | Example unit type
data Unit
  = Unit
  { unitName :: String
  }
  deriving (Eq, Ord)

-- | Shorter names
instance Show Unit where
  show u = show (unitName u)


instance Organization Unit


-- | Unit smart constructor
mkUnit :: String -> Unit
mkUnit = Unit


-- | Generate a platoon
genPlt :: String -> IO (Hierarchy Unit Echelon)
genPlt nam = do
  o1 <- mkHierarchy (mkUnit "Sq 1") Squad [] []
  o2 <- mkHierarchy (mkUnit "Sq 2") Squad [] []
  o3 <- mkHierarchy (mkUnit "Sq 3") Squad [] []
  mkHierarchy (mkUnit nam) Platoon [o1, o2, o3] []


-- | Generate a company
genCoy :: String -> IO (Hierarchy Unit Echelon)
genCoy nam = do
  o1 <- genPlt "1st"
  o2 <- genPlt "2nd"
  o3 <- genPlt "3rd"
  mkHierarchy (mkUnit nam) Company [o1, o2, o3] []


-- | Generate a battalion
genBn :: String -> IO (Hierarchy Unit Echelon)
genBn nam = do
  o1 <- genCoy "A Coy"
  o2 <- genCoy "B Coy"
  o3 <- genCoy "C Coy"
  c <-  mkHierarchy (mkUnit "Recon Plt") Platoon [] []
  mkHierarchy (mkUnit nam) Battalion [o1, o2, o3] [c]


-- | Generate a brigade
genBde :: String -> IO (Hierarchy Unit Echelon)
genBde nam = do
  o1 <- genBn "1"
  o2 <- genBn "2"
  o3 <- genBn "3"
  c <- mkHierarchy (mkUnit "Arty Coy") Company [] []
  mkHierarchy (mkUnit nam) Brigade [o1, o2, o3, c] []

