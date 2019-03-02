module Main where

import Opcon
import Opcon.Example

import Control.Concurrent

main :: IO ()
main = do
  h <- startHierarchy $ do
    -- a battalion
    bn <- lift $ genBn "101 Inf. Bn"

    -- working on the battalion from now on
    put bn

    -- a special force company
    co <- lift $ genCoy "SF Coy"

    -- a special platoon
    plt <- mkHierarchyM (mkUnit "ZZ Plt") Platoon [] []

    -- reinforce the battalion with the company
    attachHierarchyM co False

    -- locate the SF company HQ and reinforce it
    (Just hq) <- findNodeM (\u -> unitName u == "SF Coy")
    attachHierarchyToHqM plt False hq

    -- degrade some elements of the battalion
    hqs <- findAllNodesM (\u -> unitName u == "3rd")
    mapM_ detachHierarchyM hqs


  plotHierarchy h


  threadDelay 600000000
  return ()
