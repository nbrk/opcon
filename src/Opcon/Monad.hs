module Opcon.Monad
  ( editHierarchy
  , startHierarchy
  , get
  , put
  , lift
  , mkHierarchyM
  , findNodeM
  , partitionNodesM
  , findAllNodesM
  , getSuperiorM
  , getInferiorsM
  , getHqNodeM
  , getCommandM
  , getChainOfCommandM
  , detachHierarchyM
  , attachHierarchyM
  , attachHierarchyToHqM
  ) where

import Opcon.Class
import Opcon.Types
import Opcon.Instances
import Opcon.Hierarchy

import Control.Monad.State
import Control.Monad.Trans


editHierarchy :: Hierarchy a e -> HierarchyM a e ()
              -> IO (Hierarchy a e)
editHierarchy hier m =
  execStateT m hier


startHierarchy :: Organization a
                => HierarchyM a e () -> IO (Hierarchy a e)
startHierarchy m =
  editHierarchy mempty m


--
-- Some combinators on the state (for simplicity)
--
mkHierarchyM :: (Organization a, EchelonLevel e)
             => a -> e -> [Hierarchy a e]
             -> [Hierarchy a e]
             -> HierarchyM a e (Hierarchy a e)
mkHierarchyM hq hqe ors ops =
  lift $ mkHierarchy hq hqe ors ops


findNodeM  :: (a -> Bool)
           -> HierarchyM a e (Maybe (HierarchyNode a))
findNodeM = gets . findNode


-- findNodeMM :: (a -> HierarchyM a e Bool)
--            -> HierarchyM a e (Maybe (HierarchyNode a))
-- findNodeMM = 

partitionNodesM :: (a -> Bool) -> HierarchyM a e
                   ([HierarchyNode a], [HierarchyNode a])
partitionNodesM  = gets . partitionNodes


findAllNodesM :: (a -> Bool)
              -> HierarchyM a e [HierarchyNode a]
findAllNodesM = gets . findAllNodes


getSuperiorM :: Organization a
             => HierarchyNode a
             -> HierarchyM a e (HierarchyNode a)
getSuperiorM = gets . getSuperior



getInferiorsM :: Organization a => HierarchyNode a
              -> HierarchyM a e [HierarchyNode a]
getInferiorsM = gets . getInferiors


getHqNodeM :: Organization a
           => HierarchyM a e (HierarchyNode a)
getHqNodeM = gets getHqNode


getCommandM :: Organization a => HierarchyNode a
            -> HierarchyM a e (Command e)
getCommandM = gets . getCommand


getChainOfCommandM :: Organization a => HierarchyNode a
                   -> HierarchyM a e [HierarchyNode a]
getChainOfCommandM = gets . getChainOfCommand


detachHierarchyM :: Organization a
                 => HierarchyNode a
                 -> HierarchyM a e (Hierarchy a e)
detachHierarchyM n = do
  big <- get
  (d, big') <- lift $ detachHierarchy n big
  modify' (const big')
  return d


attachHierarchyToHqM :: (Organization a, EchelonLevel e)
                     => Hierarchy a e
                     -> Bool -> HierarchyNode a
                     -> HierarchyM a e ()
attachHierarchyToHqM subh organic hqn =
  modify' (attachHierarchyToHq subh organic hqn)


attachHierarchyM :: (Organization a, EchelonLevel e)
                 => Hierarchy a e -> Bool
                 -> HierarchyM a e ()
attachHierarchyM subh organic =
  modify' (attachHierarchy subh organic)
