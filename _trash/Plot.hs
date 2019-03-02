module Opcon.Plot where

import Class
import Types
import Instances

import Control.Concurrent

import Data.Graph.Visualize

-- | Plot the Hierarchy on the screen
plotHierarchy :: (Show a, Show e, Organization a)
              => Hierarchy a e -> IO ThreadId
plotHierarchy hier
  = plotDGraph hier
