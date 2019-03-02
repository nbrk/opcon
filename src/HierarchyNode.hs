module HierarchyNode where

import Class
import Types

import Data.Unique


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


