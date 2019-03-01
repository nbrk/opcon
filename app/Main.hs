module Main where

import Class
import Types
import Instances

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

data Unit
  = Unit
  { unitName :: String
  }
  deriving (Eq, Ord)
instance Show Unit where
  show u = show (unitName u)


instance Organization Unit where




main :: IO ()
main = do
  putStrLn "Lala"
