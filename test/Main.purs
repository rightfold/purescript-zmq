module Test.Main
  ( main
  ) where

import Test.Unit.Main (runTest)
import Test.Network.ZMQ as Network.ZMQ

main = runTest do
  Network.ZMQ.main
