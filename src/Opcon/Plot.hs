module Opcon.Plot where

import Opcon.Class
import Opcon.Types
import Opcon.Instances

import Control.Concurrent

import Data.Graph.Visualize

-- | Plot the Hierarchy on the screen
plotHierarchy :: (Show a, Show e, Organization a)
              => Hierarchy a e -> IO ThreadId
plotHierarchy hier
  = plotDGraphEdged hier
