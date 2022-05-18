module HSDNS.Common.Typedefs (
    DNSRequest(..),
    DNSResponse(..),
    Header(..),
    Identifier(..),
    Flags(..),
    QDCount(..),
    ANCount(..),
    NSCount(..),
    ARCount(..),
    Question(..),
    Record(..),
    Name(..),
    Label(..),
    RecordType(..),
    RecordClass(..),
    TTL(..),
    RData(..),
    Answer(..),
    Authority(..),
    Additional(..),
) where

import Data.Word (Word16, Word32)
import Data.ByteString (ByteString)


data DNSRequest = DNSRequest
    Header
    [Question]


data DNSResponse = DNSResponse
    Header
    [Question]
    [Answer]
    [Authority]
    [Additional]


data Header = Header {
    _identifier :: Identifier,
    _flags :: Flags,
    qdcount :: QDCount,
    _ancount :: ANCount,
    _nscount :: NSCount,
    _arcount :: ARCount
}

newtype Identifier = Identifier Word16

newtype Flags = Flags Word16

newtype QDCount = QDCount Word16

newtype ANCount = ANCount Word16

newtype NSCount = NSCount Word16

newtype ARCount = ARCount Word16


data Question = Question
    Name
    RecordType
    RecordClass


data Record = ARecord
    Name
    RecordType
    RecordClass
    TTL
    RData


newtype Name = Name [Label]

newtype Label = Label ByteString

newtype RecordType = RecordType Word16

newtype RecordClass = RecordClass Word16

newtype TTL = TTL Word32

newtype RData = IPAddr Word32


newtype Answer = Answer [Record]

newtype Authority = Authority [Record]

newtype Additional = Additional [Record]
