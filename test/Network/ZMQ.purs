module Test.Network.ZMQ
  ( main
  ) where

import Control.Monad.Aff (forkAff)
import Data.ByteString (toUTF8)
import Network.ZMQ
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

main = suite "Network.ZMQ" do
  test "REP/REQ" do
    context <- defaultContext
    withSocket context \(server :: Socket REP) ->
      withSocket context \(client :: Socket REQ) -> do
        bindSocket    server "inproc://server"
        connectSocket client "inproc://server"
        let parts = [toUTF8 "foo", toUTF8 "bar"]
        forkAff $ send client parts
        Assert.equal parts =<< receive server
        pure unit
    pure unit
