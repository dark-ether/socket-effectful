-- |
--  Module: Effectful.Network.Socket
--
--  This module is a wrapper around System.Socket conveniently re-exporting all needed values and functions.
module Effectful.Network.Socket (
  -- * Effect
  SocketE,

  -- ** Handlers
  runSocket,

  -- * Operations
  socket,
  connect,
  bind,
  listen,
  accept,
  send,
  sendTo,
  receive,
  receiveFrom,
  close,
  getAddress,
  sendAll,
  sendAllLazy,
  sendAllBuilder,
  effSetSocketOption,
  effGetSocketOption,

  -- * Re-exports
  S.Socket,
  S.MessageFlags (..),
  S.Family (..),
  S.Type (..),
  S.Protocol (..),
  module System.Socket.Family.Inet,
  module System.Socket.Family.Inet6,
  module System.Socket.Protocol.Default,
  module System.Socket.Protocol.TCP,
  module System.Socket.Protocol.UDP,
  module System.Socket.Type.Datagram,
  module System.Socket.Type.Raw,
  module System.Socket.Type.SequentialPacket,
  S.Stream,
  S.AddressInfo (..),
  S.HasAddressInfo (..),
  S.NameInfo (..),
  S.SocketOption (..),
  S.Error (..),
  S.ReuseAddress (..),
  S.KeepAlive (..),
  S.msgNoSignal,
  S.msgPeek,
  S.AddressInfoFlags,
  S.aiAddressConfig,
  S.aiAll,
  S.aiCanonicalName,
  S.aiNumericHost,
  S.aiNumericService,
  S.aiPassive,
  S.aiV4Mapped,
  S.NameInfoFlags,
  S.niNameRequired,
  S.niDatagram,
  S.niNoFullyQualifiedDomainName,
  S.niNumericHost,
  S.niNumericService,
  S.SocketException,
  S.eOk,
  S.eInterrupted,
  S.eBadFileDescriptor,
  S.ePermissionDenied,
  S.eInvalid,
  S.ePipe,
  S.eWouldBlock,
  S.eAgain,
  S.eNotSocket,
  S.eDestinationAddressRequired,
  S.eMessageSize,
  S.eProtocolType,
  S.eNoProtocolOption,
  S.eProtocolNotSupported,
  S.eSocketTypeNotSupported,
  S.eOperationNotSupported,
  S.eProtocolFamilyNotSupported,
  S.eAddressFamilyNotSupported,
  S.eAddressInUse,
  S.eAddressNotAvailable,
  S.eNetworkDown,
  S.eNetworkUnreachable,
  S.eNetworkReset,
  S.eConnectionAborted,
  S.eConnectionReset,
  S.eNoBufferSpace,
  S.eIsConnected,
  S.eNotConnected,
  S.eShutdown,
  S.eTooManyReferences,
  S.eTimedOut,
  S.eConnectionRefused,
  S.eHostDown,
  S.eHostUnreachable,
  S.eAlready,
  S.eInProgress,
  S.AddressInfoException,
  S.eaiAgain,
  S.eaiBadFlags,
  S.eaiFail,
  S.eaiFamily,
  S.eaiMemory,
  S.eaiNoName,
  S.eaiSocketType,
  S.eaiService,
  S.eaiSystem,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Effectful
import Effectful.Dispatch.Static
import System.Socket qualified as S
import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Protocol.Default
import System.Socket.Protocol.TCP
import System.Socket.Protocol.UDP
import System.Socket.Type.Datagram
import System.Socket.Type.Raw
import System.Socket.Type.SequentialPacket
import System.Socket.Type.Stream (Stream)
import System.Socket.Type.Stream qualified as S


-- | runs the 'SocketE' Effect
runSocket :: IOE :> es => Eff (SocketE : es) a -> Eff es a
runSocket = evalStaticRep SocketERep


-- | lifted 'System.Socket.socket'
socket :: (SocketE :> es, S.Family f, S.Type t, S.Protocol p) => Eff es (S.Socket f t p)
socket = unsafeEff_ S.socket


-- | lifted 'System.Socket.connect'
connect :: (SocketE :> es, S.Family f) => S.Socket f t p -> SocketAddress f -> Eff es ()
connect sock addr = unsafeEff_ $ S.connect sock addr


-- | lifted 'System.Socket.bind'
bind :: (SocketE :> es, S.Family f) => S.Socket f t p -> SocketAddress f -> Eff es ()
bind sock addr = unsafeEff_ $ S.bind sock addr


-- | lifted 'System.Socket.listen'
listen :: (SocketE :> es) => S.Socket f t p -> Int -> Eff es ()
listen sock int = unsafeEff_ $ S.listen sock int


-- | lifted 'System.Socket.accept'
accept :: (SocketE :> es, S.Family f) => S.Socket f t p -> Eff es (S.Socket f t p, SocketAddress f)
accept = unsafeEff_ . S.accept


-- | lifted 'System.Socket.send'
send :: (SocketE :> es) => S.Socket f t p -> ByteString -> S.MessageFlags -> Eff es Int
send sock mess opts = unsafeEff_ $ S.send sock mess opts


-- | lifted 'System.Socket.sendTo'
sendTo :: (SocketE :> es, S.Family f) => S.Socket f t p -> ByteString -> S.MessageFlags -> SocketAddress f -> Eff es Int
sendTo sock mess opts to = unsafeEff_ $ S.sendTo sock mess opts to


-- | lifted 'System.Socket.receive'
receive :: (SocketE :> es) => S.Socket f t p -> Int -> S.MessageFlags -> Eff es ByteString
receive sock int opts = unsafeEff_ $ S.receive sock int opts


-- | lifted 'System.Socket.receiveFrom'
receiveFrom :: (SocketE :> es, S.Family f) => S.Socket f t p -> Int -> S.MessageFlags -> Eff es (ByteString, SocketAddress f)
receiveFrom sock int opts = unsafeEff_ $ S.receiveFrom sock int opts


-- | lifted 'System.Socket.close'
close :: (SocketE :> es) => S.Socket f t p -> Eff es ()
close = unsafeEff_ . S.close


-- | lifted 'System.Socket.getAddress'
getAddress :: (SocketE :> es, S.Family f) => S.Socket f t p -> Eff es (SocketAddress f)
getAddress = unsafeEff_ . S.getAddress


-- | lifted 'System.Socket.Type.Stream.sendAll'
sendAll :: (SocketE :> es) => S.Socket f Stream p -> ByteString -> S.MessageFlags -> Eff es Int
sendAll sock mess opts = unsafeEff_ $ S.sendAll sock mess opts


-- | lifted 'System.Socket.Type.Stream.sendAllLazy'
sendAllLazy :: (SocketE :> es) => S.Socket f Stream p -> BSL.ByteString -> S.MessageFlags -> Eff es Int64
sendAllLazy sock mess opts = unsafeEff_ $ S.sendAllLazy sock mess opts


-- | lifted 'System.Socket.Type.Stream.sendAllBuilder'
sendAllBuilder :: (SocketE :> es) => S.Socket f Stream p -> Int -> Builder -> S.MessageFlags -> Eff es Int64
sendAllBuilder sock int build opts = unsafeEff_ $ S.sendAllBuilder sock int build opts


-- | lifted 'System.Socket.getSocketOption'
effGetSocketOption :: (SocketE :> es, S.SocketOption o) => S.Socket f t p -> Eff es o
effGetSocketOption sock = unsafeEff_ $ S.getSocketOption sock


-- | lifted 'System.Socket.setSocketOption'
effSetSocketOption :: (SocketE :> es, S.SocketOption o) => S.Socket f t p -> o -> Eff es ()
effSetSocketOption sock opt = unsafeEff_ $ S.setSocketOption sock opt


-- | The socket effect
type SocketE :: Effect
data SocketE m a


type instance DispatchOf SocketE = 'Static 'WithSideEffects


data instance StaticRep SocketE = SocketERep
