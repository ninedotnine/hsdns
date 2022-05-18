module HSDNS.Server.Network (
    with_datagram_socket    -- opens a socket
                            -- runs a given function,
                            -- closes the socket
) where

import Control.Exception qualified as Exception (bracket)
import Data.Function
import Network.Socket (
    addrAddress,
    addrFamily,
    addrFlags,
    AddrInfo,
    AddrInfoFlag(AI_PASSIVE),
    addrSocketType,
    bind,
    close,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    setCloseOnExecIfNeeded,
    SocketType(Datagram),
    Socket,
    socket,
    SocketType(Stream),
    withFdSocket,
    )

import HSDNS.Settings qualified as Settings (service_name)


with_datagram_socket :: (Socket -> IO a) -> IO a
with_datagram_socket process_requests = do
    addr_info <- resolve
    Exception.bracket (open addr_info) close process_requests

resolve :: IO AddrInfo
resolve = do
    -- getAddrInfo promises to never return [], see
    -- https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html#g:2
    head <$> getAddrInfo
                (Just hints)
                Nothing
                (Just Settings.service_name)
        where
            hints = defaultHints {
                addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
            }

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket
                (addrFamily addr)
                Datagram
                defaultProtocol
    setCloseOnExecIfNeeded & withFdSocket sock
    addr & addrAddress & (sock & bind)
    pure sock
