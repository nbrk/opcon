module Hierarchy where

import Class
import Types
import Instances
import HierarchyNode
import Command

import Control.Monad
import Data.Unique
import Data.Graph.DGraph
import Data.Graph.Types
import Data.Maybe
import qualified Data.List as L



-- | Check if the echelon is superior to the given hiers
echelonSuperior :: (Organization a, EchelonLevel e)
             => e -> [Hierarchy a e] -> Bool
echelonSuperior hqe hiers =
  let es = map (echelonOf . getHqCommand) hiers
  in
    all (\e -> hqe > e) es


-- | Check if the echelon is superior to the given hiers
echelonSuperiorOrEqual :: (Organization a, EchelonLevel e)
             => e -> [Hierarchy a e] -> Bool
echelonSuperiorOrEqual hqe hiers =
  let es = map (echelonOf . getHqCommand) hiers
  in
    all (\e -> hqe >= e) es


-- | Construct a hier with organic/opcon subordinate hiers.
--   Conveniently uses `IO` to generate Unique node hashes.
mkHierarchy :: (Organization a, EchelonLevel e)
            => a -> e -> [Hierarchy a e] -> [Hierarchy a e]
            -> IO (Hierarchy a e)
mkHierarchy hq hqe organichs opconhs = do
  ean <- mkHierarchyNode Nothing -- ghost EA node
  hqn <- mkHierarchyNode (Just hq)
  return $
    mkHierarchyWith ean hqe hqn organichs opconhs


-- | Construct a hier with organic/opcon subordinate hiers.
--   All of the provided nodes MUST already have unique
--   hashes (i.e. `hierarchyNodeHash`) set up.
mkHierarchyWith :: (Organization a, EchelonLevel e)
                => HierarchyNode a -> e
                -> HierarchyNode a -> [Hierarchy a e]
                -> [Hierarchy a e] -> Hierarchy a e
mkHierarchyWith ean hqe hqn organichs opconhs =
  -- XXX BUG
  let eaArc = Arc ean hqn (Organic hqe)
      organicArcs = mkArcs hqn organichs toOrganic
      opconArcs = mkArcs hqn opconhs toOpcon
      mkArcs hqn hiers f =
        concatMap
          (\h -> let cmd = getHqCommand h
                 in
                   arcs (replaceEANode hqn (f cmd) h)
          )
          hiers
  in
    if (not (echelonSuperior hqe organichs)
       || not (echelonSuperior hqe opconhs))
    then
      error $ "broken chain of command"
        ++ " (subordinates of higher echelons)"
    else
      fromArcsList $ eaArc : organicArcs ++ opconArcs




-- | Get the HQ node (of highest echelon) of the hierarchy
getHqNode :: Organization a
          => Hierarchy a e -> HierarchyNode a
getHqNode hier =
  case arcs hier of
    [] -> error "getHqNode: no command connections"
    eaArc:_ -> destinationVertex eaArc


-- | Get the hier's HQ command link type and echelon
getHqCommand :: Organization a =>Hierarchy a e -> Command e
getHqCommand hier =
  case arcs hier of
    [] -> error "getHqCommand: no command connections"
    eaArc:_ -> attribute eaArc


-- | Get the "Echelon Above" ghost node of the hier
getEANode :: Organization a
          => Hierarchy a e -> HierarchyNode a
getEANode hier =
  let srcns = filter (isSource hier) (vertices hier)
  in
    case srcns of
      [] -> error "getEANode: no EA node"
      _:_:_ -> error "getEANode: many EA nodes"
      ean:[] -> ean


-- | Replace EA ghost node of the hier with the given one
replaceEANode :: (Organization a, EchelonLevel e)
          => HierarchyNode a -> Command e -> Hierarchy a e
          -> Hierarchy a e
replaceEANode n cmd hier =
  let ean = getEANode hier
  in
    if echelonSuperiorOrEqual (echelonOf cmd) [hier]
    then
      let eaarcs = outboundingArcs hier ean
      in
        case eaarcs of
          [] -> error "replaceEANode: no EA command"
          _:_:_ -> error "replaceEANode: many EA commands"
          eaarc@(Arc _fr to cmd):[] ->
              insertArc
                (Arc n to cmd)
                (removeArc eaarc hier)
    else error "replaceEANode: new echelon is too low"



-- | Find the first node satisfying the predicate
findNode :: (a -> Bool) -> Hierarchy a e
         -> Maybe (HierarchyNode a)
findNode pred hier =
  let ns = vertices hier
  in
    L.find
      (\n -> let dat = hierarchyNodeData n
             in
               isJust dat && pred (fromJust dat)
      )
      ns


-- | Partition nodes wrt the predicate
partitionNodes :: (a -> Bool) -> Hierarchy a e
               -> ([HierarchyNode a], [HierarchyNode a])
partitionNodes pred hier =
  let ns = vertices hier
  in
    L.partition
      (\n -> let dat = hierarchyNodeData n
             in
               isJust dat && pred (fromJust dat)
      )
      ns


-- | Find all nodes that satisfy the predicate
findNodes :: (a -> Bool) -> Hierarchy a e
             -> [HierarchyNode a]
findNodes pred hier =
  fst $ partitionNodes pred hier


-- | Get the superior (i.e. the commander) of a node
getSuperior :: Organization a
            => HierarchyNode a -> Hierarchy a e
            -> Maybe (HierarchyNode a)
getSuperior n hier =
  case inboundingArcs hier n of
    [] -> Nothing
    a:_ -> Just (originVertex a)


-- | Get all inferiors (i.e. subordinates) of the node
getInferiors :: Organization a
            => HierarchyNode a -> Hierarchy a e
            -> [HierarchyNode a]
getInferiors n hier =
  case outboundingArcs hier n of
    [] -> []
    as -> map destinationVertex as
