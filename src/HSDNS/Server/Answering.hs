module HSDNS.Server.Answering (
    answered    -- generates a suitable DNSResponse
                -- for a given DNSRequest
) where

import Data.Bits (setBit, clearBit, (.&.))
import Data.Functor
import Data.Function

import HSDNS.Common.Packing (pack32)
import HSDNS.Common.Typedefs


answered :: DNSRequest -> DNSResponse
answered (DNSRequest header questions) =
    DNSResponse
        (response_header header)
        questions
        (answers questions)
        default_authorities
        default_additionals


response_header :: Header -> Header
response_header (Header ident flags (QDCount qd) _ _ _) =
    Header
        ident
        (response_flags flags)
        (QDCount qd)
        (ANCount qd) -- as many answers as questions
        (NSCount 0)
        (ARCount 0)


response_flags :: Flags -> Flags
response_flags = set_query_bit
             <&> clear_aa_bit
             <&> clear_second_byte  -- the RA bit, reserved bits,
                                    -- and error bits are all zero
    where
        set_query_bit (Flags bits) = Flags (setBit bits 15)
        clear_aa_bit (Flags bits) = Flags (clearBit bits 10)
--         clear_ra_bit (Flags bits) = Flags (clearBit bits 7)
        clear_second_byte (Flags bits) = Flags (bits .&. 0xFF00)


answers :: [Question] -> [Answer]
answers = map answer
    where
        -- every question gets the same answer.
        -- we don't do "real" DNS resolving
        answer :: Question -> Answer
        answer (Question name t c) = Answer [
            ARecord name t c default_TTL default_IP
            ]


-- they could cache it forever;
-- as long as they keep asking this resolver,
-- the answer isn't going to change!
-- anyway, 6666 is about two hours;
-- that seems reasonable.
default_TTL :: TTL
default_TTL = TTL 6666


-- all requests get the same response.
-- sinister indeed!
default_IP :: RData
default_IP = IPAddr (pack32 6 6 6 6)


default_authorities :: [Authority]
default_authorities = []


default_additionals :: [Additional]
default_additionals = []
