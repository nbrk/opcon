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
import Data.Graph.Traversal
import Data.Maybe
import qualified Data.List as L



-- | Check if the echelon level is superior to the given
--   hierarchy. I.e. an officer of rank `e` could take
--   the command of the hierarchy `hier`.
echelonSuperior :: (Organization a, EchelonLevel e)
             => e -> Hierarchy a e -> Bool
echelonSuperior e hier =
  let hqe = getCommand (getHqNode hier) hier
  in
    e > echelonLevel hqe


-- | Check if the echelon level is equal to that of the
--   given hierarchy's hq.
echelonEqual :: (Organization a, EchelonLevel e)
             => e -> Hierarchy a e -> Bool
echelonEqual e hier =
  let hqe = getCommand (getHqNode hier) hier
  in
    e == echelonLevel hqe


-- | Construct a military hierarchy with organic and/or
--   OPCON subordinate hierarchies.
mkHierarchy :: (Organization a, EchelonLevel e)
            => a -> e -> [Hierarchy a e]
            -> [Hierarchy a e] -> IO (Hierarchy a e)
mkHierarchy hq hqe organichs opconhs = do
  ean <- mkHierarchyNode Nothing -- ghost EA node
  hqn <- mkHierarchyNode (Just hq) -- this hq node
  return $
    mkHierarchyWith ean hqe hqn organichs opconhs


-- | Construct a military hierarchy with organic and/or
--   OPCON subordinate hierarchies.
--   All of the provided nodes MUST already have unique
--   hashes (i.e. `hierarchyNodeHash`) set up.
mkHierarchyWith :: (Organization a, EchelonLevel e)
                => HierarchyNode a -> e
                -> HierarchyNode a -> [Hierarchy a e]
                -> [Hierarchy a e] -> Hierarchy a e
mkHierarchyWith ean hqe hqn organichs opconhs =
  let eaArc = Arc ean hqn (Organic hqe)
      organicArcs = mkArcs hqn organichs toOrganic
      opconArcs = mkArcs hqn opconhs toOpcon
      mkArcs hqn hiers f =
        concatMap
          (\h -> let cmd = getCommand (getHqNode h) h
                 in
                   arcs (replaceEANode hqn (f cmd) h)
          )
          hiers
  in
    if not $
       ( and (fmap (echelonSuperior hqe) organichs)
       || and (fmap (echelonSuperior hqe) opconhs))
    then
      error $ "broken chain of command"
        ++ " (subordinates of higher echelons)"
    else
      fromArcsList $ eaArc : organicArcs ++ opconArcs




-- | Get the HQ, i.e. the node of highest echelon of the
--   hierarchy (ecluding the EA)
getHqNode :: Organization a
          => Hierarchy a e -> HierarchyNode a
getHqNode hier =
  let ean = getEANode hier
  in
    case outboundingArcs hier ean of
      [] -> error "getHqNode: no command from EA"
      _:_:_ -> error "getHqNode: many commands from EA"
      a:[] -> destinationVertex a


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
    if echelonSuperior (echelonLevel cmd) hier
       || echelonEqual (echelonLevel cmd) hier
    then
      let eaarcs = outboundingArcs hier ean
      in
        case eaarcs of
          [] -> error "replaceEANode: no EA command"
          _:_:_ -> error "replaceEANode: many EA commands"
          eaarc@(Arc _fr to _cmd):[] ->
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
findAllNodes :: (a -> Bool) -> Hierarchy a e
             -> [HierarchyNode a]
findAllNodes pred hier =
  fst $ partitionNodes pred hier


-- | Get the superior (i.e. the commander) of a node
getSuperior :: Organization a
            => HierarchyNode a -> Hierarchy a e
            -> HierarchyNode a
getSuperior n hier =
  case inboundingArcs hier n of
    [] -> n -- the EA node
    a:_ -> originVertex a


-- | Get the command uplink of the node (i.e. the
--   connection to the node's superior officer)
getCommand :: Organization a
           => HierarchyNode a -> Hierarchy a e
           -> Command e
getCommand n hier =
  let as = inboundingArcs hier n
  in
    case as of
      [] -> error "getCommand: no command connections"
      _:_:_ ->error "getCommand: many command connections"
      (Arc _ _ cmd):[] -> cmd



-- | Get all inferiors (i.e. subordinates) of the node
getInferiors :: Organization a
            => HierarchyNode a -> Hierarchy a e
            -> [HierarchyNode a]
getInferiors n hier =
  case outboundingArcs hier n of
    [] -> []
    as -> map destinationVertex as


-- | Get the Chain of Command up to the topmost echelon
--   level, excluding the EA node
getChainOfCommand :: Organization a
                  => HierarchyNode a -> Hierarchy a e
                  -> [HierarchyNode a]
getChainOfCommand n hier =
  let supn = getSuperior n hier
  in
    if isEANode supn
    then []
    else supn : getChainOfCommand supn hier


-- | Detach a sub-hierarchy from the big hierarchy.
--   Returns the detachment and the remains of the force.
--   All of the provided nodes MUST already have unique
--   hashes (i.e. `hierarchyNodeHash`) set up.
detachHierarchyWith :: Organization a
                    => HierarchyNode a -> HierarchyNode a
                    -> Hierarchy a e
                    -> (Hierarchy a e, Hierarchy a e)
detachHierarchyWith detean n hier =
  let detsupn = getSuperior n hier
      detcmd = getCommand n hier

      detns = bfsVertices hier n
      vves = concatMap (incidentEdgeTriples hier) detns
      detas = map
               (\(n1, n2, c) -> Arc n1 n2 c)
               vves
      dethier =
        removeVertex detsupn $
          fromArcsList $
            (Arc detean n (toOrganic detcmd)) : detas
      hier' = removeVertices detns hier
  in
    (dethier, hier')


-- | Detach a sub-hierarchy from the big hierarchy.
--   Returns the detachment and the remains of the force.
detachHierarchy :: Organization a
                => HierarchyNode a -> Hierarchy a e
                -> IO (Hierarchy a e, Hierarchy a e)
detachHierarchy n hier = do
  detean <- mkHierarchyNode Nothing -- ghost EA node
  return $
    detachHierarchyWith detean n hier


-- | Attach a sub-hierarchy to the given node (hq) inside
--   of a hierarchy. The given hq-node's echelon must be
--   superior to the attaching force.
attachHierarchy :: (Organization a, EchelonLevel e)
                => Hierarchy a e -> Bool ->HierarchyNode a
                -> Hierarchy a e -> Hierarchy a e
attachHierarchy subh organic hqn hier =
  let hierhqe = echelonLevel $ getCommand hqn hier
      subhhqe =
        echelonLevel $ getCommand (getHqNode subh) subh
      conn = if organic then Organic else Opcon
  in
    if not (echelonSuperior hierhqe subh)
    then
      error $
        "attachHierarchy: echelon too high for the HQ: "
        ++ show subhhqe ++ " > " ++ show hierhqe
    else
      let subh' = replaceEANode hqn (conn subhhqe) subh
      in
        insertArcs (arcs subh') hier
