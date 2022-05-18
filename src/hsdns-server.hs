import Control.Monad (forever)
import Data.Function ((&))
import Data.Void (Void)
import Network.Socket (Socket)
import Network.Socket.ByteString (recvFrom, sendAllTo)

import HSDNS.Common.Decoding (decoded)
import HSDNS.Common.Encoding (encoded)
import HSDNS.Server.Answering (answered)
import HSDNS.Server.Network (with_datagram_socket)


main :: IO Void
main = with_datagram_socket process_requests


max_msg_length = 512


-- run forever, answering requests,
-- reusing the same socket
process_requests :: Socket -> IO Void
process_requests sock = forever $ do
    (msg, peer) <- recvFrom sock max_msg_length
    case decoded msg of
        Left _parse_err -> pure () -- drop the message, send no response
        Right req -> sendAllTo sock response peer
            where response = req
                           & answered
                           & encoded
--         Right req -> do
--             putStrLn "--- received ---"
--             print (req)
--             putStrLn "--- sending ---"
--             print (answered req)
--             let response = req & answered & encoded
--             sendAllTo sock response peer

