module Opcon.HierarchyNode where

import Class
import Types

import Data.Unique
import Data.Maybe


-- | Construct a new node (maybe with no data) and hash
--   it by a guaranteed unique integer in the `IO` monad.
mkHierarchyNode :: Organization a => Maybe a
                -> IO (HierarchyNode a)
mkHierarchyNode a = do
  u <- newUnique
  return $
    mkHierarchyNodeWith a (hashUnique u)


-- | Purely construct a node with the give hash integer.
--   The caller must guarantee uniqueness of the hash.
mkHierarchyNodeWith :: Organization a => Maybe a -> Int
                    -> HierarchyNode a
mkHierarchyNodeWith a hash =
  HierarchyNode
  { hierarchyNodeData = a
  , hierarchyNodeHash = hash
  }


-- | Is this a ghost EA (Echelons Above) node?
isEANode :: HierarchyNode a -> Bool
isEANode n = isNothing $ hierarchyNodeData n


-- | Construct showable path for the given chain of command
mkStringPath :: Show a => [HierarchyNode a] -> [String]
mkStringPath supns =
  map
    ( \n ->
        show (fromJust (hierarchyNodeData n))
    )
    supns
