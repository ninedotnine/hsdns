module HSDNS.Common.Encoding (
    encoded -- encodes a DNSResponse as a ByteString
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (fold)
import Data.Function
import Data.Functor
import Data.Word (Word8, Word16, Word32)

import HSDNS.Common.Typedefs
import HSDNS.Common.Packing


encoded :: DNSResponse -> ByteString
encoded (DNSResponse h qs anss auths adds) =
    fold [ header h,
           questions qs,
           answers anss,
           authorities auths,
           additionals adds
           ]


header :: Header -> ByteString
header = \case
    Header
        (Identifier i)
        (Flags flags)
        (QDCount qd)
        (ANCount an)
        (NSCount ns)
        (ARCount ar)
            -> [encode_word16 i,
                encode_word16 flags,
                encode_word16 qd,
                encode_word16 an,
                encode_word16 ns,
                encode_word16 ar] & fold


questions :: [Question] -> ByteString
questions = map question
        <&> fold
    where
        question :: Question -> ByteString
        question (Question labels typ cla) =
            fold [ name labels,
                   record_type typ,
                   record_class cla
                 ]


answers :: [Answer] -> ByteString
answers = map answer
      <&> fold


authorities :: [Authority] -> ByteString
authorities = map authority
          <&> fold
    where
        -- because we do not insert any data
        -- for the authority section,
        -- `authorities` will only ever be passed an empty list.
        -- `authority` will never be called at all.
        -- a more sophisticated DNS resolver
        -- would need a way to encode authority data.
        -- that's a task for the future.
        authority :: Authority -> ByteString
        authority = error "unreachable"


additionals :: [Additional] -> ByteString
additionals = map additional
          <&> fold
    where
        -- see the note above for `authorities`.
        additional :: Additional -> ByteString
        additional = error "unreachable"


answer :: Answer -> ByteString
answer (Answer recs) = records recs


records :: [Record] -> ByteString
records = map record
      <&> fold


record :: Record -> ByteString
record = \case
    ARecord n t c (TTL seconds) rdata ->
       fold [ name n,
              record_type t,
              record_class c,
              encode_word32 seconds,
              length_and_record rdata
              ]


length_and_record :: RData -> ByteString
length_and_record = \case
    -- prepend the 16-bit length,
    -- which for an IP address is always 4
    IPAddr addr -> encode_word16 4 <> encode_word32 addr


record_type :: RecordType -> ByteString
record_type (RecordType bytes) = encode_word16 bytes


record_class :: RecordClass -> ByteString
record_class (RecordClass bytes) = encode_word16 bytes


name :: Name -> ByteString
name (Name labels) = labels
                   & map label
                   & fold
                   & (<> "\0")  -- list of names must be terminated
                                -- with a null byte


label :: Label -> ByteString
label (Label word) = BS.cons len word
    where
        -- a label is only permitted to have 63 octets
        len :: Word8
        len = word & BS.length & fromIntegral


encode_word16 :: Word16 -> ByteString
encode_word16 w16 = BS.pack [x,y]
    where (x,y) = unpack16 w16


encode_word32 :: Word32 -> ByteString
encode_word32 w32 = BS.pack [w,x,y,z]
    where (w,x,y,z) = unpack32 w32
