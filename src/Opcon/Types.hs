module Opcon.Types where

import Opcon.Class

import Data.Graph.DGraph
import Control.Monad.State


-- | Type of command connection
data Command e
  = Organic e
  | Opcon e
  deriving (Eq, Show)


-- | A node contains the data and guarantees uniqueness
data HierarchyNode a
  = HierarchyNode
  { hierarchyNodeData :: Maybe a
  , hierarchyNodeHash :: Int
  } deriving (Eq, Show)


-- | The military hierarchy of formations of
--   type `a` divided by echelons of type `e`
type Hierarchy a e = DGraph (HierarchyNode a) (Command e)


-- | State monad for sequential manipulations on a hier
type HierarchyM a e = StateT (Hierarchy a e) IO
