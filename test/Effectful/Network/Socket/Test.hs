module Effectful.Network.Socket.Test where

import Control.Monad((<=<))
import Control.Monad.Catch
import Data.ByteString
import Effectful
import Effectful.Network.Socket
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic


connectsAndReceives :: ByteString -> SocketE :> es => Eff es Bool
connectsAndReceives message = do
  bracket
    ( do
        sock <- socket :: SocketE :> es => Eff es (Socket Inet Stream TCP)
        effSetSocketOption sock $ ReuseAddress True
        return sock
    )
    close
    ( \s -> do
        bind s $ SocketAddressInet (inetAddressFromTuple (127, 0, 0, 1)) 8080
        listen s 5
        bracket
          ( do
              sock <- socket :: SocketE :> es => Eff es (Socket Inet Stream TCP)
              effSetSocketOption sock $ ReuseAddress True
              return sock
          )
          close
          ( \client -> do
              connect client $ SocketAddressInet (inetAddressFromTuple (127, 0, 0, 1)) 8080
              (clientSV, _) <- accept s
              _ <- send clientSV message mempty
              rmess <- receive client 4096 mempty
              pure $ rmess == message
          )
    )


prop_car :: Property
prop_car = verbose . ioProperty . ((runEff . runSocket) <=< generate . monadic') $ do
  bs <- pick arbitrary
  prop <- run $ connectsAndReceives bs
  assert prop

