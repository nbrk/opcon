module Main where

import Opcon
import Opcon.Example

import Control.Concurrent

main :: IO ()
main = do
  -- a battalion
  bn <- genBn "101 Inf. Bn"

  -- a special force company
  co <- genCoy "SF Coy"

  -- reinforce the battalion
  let bn' = attachHierarchy co False bn

  plotHierarchy bn'


  threadDelay 600000000
  return ()
