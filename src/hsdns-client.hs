-- silly demo client
-- does not even read or parse the response,
-- because there's no parser for DNSResponses
import Data.ByteString (ByteString)

import HSDNS.Client.Network (send_msg)
import HSDNS.Common.Typedefs
import HSDNS.Common.Packing
import HSDNS.Common.Encoding (encoded)
import HSDNS.Settings qualified as Settings (service_name)


target_host = "127.0.0.1"


main :: IO ()
main = do
    putStrLn "sending message"
    send_msg target_host Settings.service_name sample_text


sample_text :: ByteString
sample_text = encoded sample_request


-- contrary to its type, this is a request.
-- it has type DNSResponse because requests and responses
-- are compatible (provided there are no answers)
-- and I did not write an encode function for DNSRequest.
sample_request :: DNSResponse
sample_request = DNSResponse header qs anss auths adds
    where
        header :: Header
        header = Header
            (Identifier (pack16 42 42))
            (Flags 0b_0000_0001_0010_0000)
            (QDCount 1)
            (ANCount 0)
            (NSCount 0)
            (ARCount 0)
        qs = [Question
                (Name [
                    Label "danso",
                    Label "ca"
                ])
                (RecordType  1)
                (RecordClass 1)
             ]
        anss = []
        auths = []
        adds = []
