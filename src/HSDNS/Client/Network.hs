module HSDNS.Client.Network (
    send_msg, -- send a single message to a specified host and port
) where

import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Network.Socket (
    addrSocketType,
    addrAddress,
    addrFamily,
    AddrInfo,
    connect,
    close,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    HostName,
    ServiceName,
    socket,
    Socket,
    SocketType(Stream, Datagram),
    )
import Network.Socket.ByteString (sendAll)


send_msg :: HostName -> ServiceName -> ByteString -> IO ()
send_msg host port msg = do
    addr_info <- resolve host port
    Exception.bracket (open addr_info) close (`sendAll` msg)

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    -- getAddrInfo promises to never return [], see
    -- https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html#g:2
    head <$> getAddrInfo
                (Just hints)
                (Just host)
                (Just port)
        where
            hints = defaultHints {
                addrSocketType = Stream
            }

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket
        (addrFamily addr)
        Datagram
        defaultProtocol
    connect sock $ addrAddress addr
    pure sock
