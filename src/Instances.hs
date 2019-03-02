{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Instances where

import Class
import Types

import Data.GraphViz.Printing
import Data.Hashable
import Data.Text.Lazy
import Data.Bits


-- | Unique hash of a node
instance Organization a => Hashable (HierarchyNode a) where
  hash n = hierarchyNodeHash n
  hashWithSalt salt n = salt `combine` hash n
    where
      combine h1 h2 = (h1 * 16777619) `xor` h2



-- | Nodes as dot objects
instance Organization a => PrintDot (HierarchyNode a) where
  unqtDot (HierarchyNode (Just a) h) =
    let t = pack $ (show a) ++ " (" ++ show h ++ ")"
    in
      addQuotes (pack "\'") $ unqtText t
  unqtDot (HierarchyNode Nothing h) =
    let t = pack $ "*** EA ***" ++ " (" ++ show h ++ ")"
    in
      addQuotes (pack "\'") $ unqtText t


instance Organization String where
