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


-- | Check if the echelon is superior to the given hiers
superiorOver :: (Organization a, EchelonLevel e)
             => e -> [Hierarchy a e] -> Bool
superiorOver hqe hiers =
  let es = map (echelonOf . getHqCommand) hiers
  in
    all (\e -> hqe > e) es


-- | Construct a hier with organic/opcon subordinate hiers.
--   Conveniently uses `IO` to generate Unique node hashes.
mkHierarchy :: (Organization a, EchelonLevel e)
            => a -> e -> [Hierarchy a e] -> [Hierarchy a e]
            -> IO (Hierarchy a e)
mkHierarchy hq hqe organichs opconhs = do
  ean <- mkHierarchyNode Nothing -- ghost EA node
  hqn <- mkHierarchyNode (Just hq)
  return $
    mkHierarchyWith ean (Organic hqe) hqn organichs opconhs


-- | Construct a hier with organic/opcon subordinate hiers.
--   All of the provided nodes MUST already have unique
--   hashes (i.e. `hierarchyNodeHash`) set up.
mkHierarchyWith :: (Organization a, EchelonLevel e)
                => HierarchyNode a -> Command e
                -> HierarchyNode a -> [Hierarchy a e]
                -> [Hierarchy a e] -> Hierarchy a e
mkHierarchyWith ean hqcmd hqn organichs opconhs =
  let eaArc = Arc ean hqn hqcmd
      organicArcs = mkArcs hqn organichs
      opconArcs = mkArcs hqn opconhs
      mkArcs hqn hiers = concatMap
                         (\h -> arcs (replaceEA hqn h))
                         hiers
  in
    if (not (superiorOver (echelonOf hqcmd) organichs)
       || not (superiorOver (echelonOf hqcmd) opconhs))
    then
      error $ "broken chain of command"
        ++ " (subordinates of higher echelons)"
    else
      fromArcsList $ eaArc : organicArcs ++ opconArcs



-- | Get the HQ node (of highest echelon) of the hierarchy
getHq :: Organization a
          => Hierarchy a e -> HierarchyNode a
getHq hier =
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
getEA :: Organization a => Hierarchy a e -> HierarchyNode a
getEA hier =
  case arcs hier of
    [] -> error "getEA: no command connections"
    eaArc:_ -> originVertex eaArc


-- | Replace EA ghost node of the hier with the given one
replaceEA :: Organization a
          => HierarchyNode a -> Hierarchy a e
          -> Hierarchy a e
replaceEA n hier =
  let (Arc fr to e):as = arcs hier
  in
    fromArcsList $ (Arc n to e):as

