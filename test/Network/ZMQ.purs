module Test.Network.ZMQ
  ( main
  ) where

import Network.ZMQ
import Prelude
import Test.Unit (suite, test)

main = suite "Network.ZMQ" do
  test "bind and connect" do
    context <- defaultContext
    withSocket context \(server :: Socket REP) ->
      withSocket context \(client :: Socket REQ) -> do
        bindSocket    server "inproc://server"
        connectSocket client "inproc://client"
    pure unit
