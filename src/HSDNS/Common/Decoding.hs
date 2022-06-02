module HSDNS.Common.Decoding (
    decoded -- parses a ByteString into a DNSRequest,
            -- or Left on failure with an error message
) where

import Prelude hiding (take)

import Data.ByteString (ByteString)
import Data.Word (Word16)
import Data.Attoparsec.ByteString (
    Parser,
    anyWord8,
    count,
    manyTill,
    parseOnly,
    take,
    word8
    )

import HSDNS.Common.Typedefs
import HSDNS.Common.Packing


decoded :: ByteString -> Either String DNSRequest
decoded = parseOnly dns_request


dns_request :: Parser DNSRequest
dns_request = do
    h <- header
    qs <- questions (qdcount h)
    pure $ (DNSRequest h qs)
    -- there may be additional data,
    -- but for our purposes it will be ignored.


header :: Parser Header
header = do
    i <- Identifier <$> anyWord16
    fls <- Flags  <$> anyWord16
    qd <- QDCount <$> anyWord16
    an <- ANCount <$> anyWord16
    ns <- NSCount <$> anyWord16
    ar <- ARCount <$> anyWord16
    pure $ Header i fls qd an ns ar


questions :: QDCount -> Parser [Question]
questions (QDCount n_questions) = count (fromIntegral n_questions) question


question :: Parser Question
question = Question <$> name <*> record_type <*> record_class


name :: Parser Name
name = Name <$> manyTill label (word8 0)


label :: Parser Label
label = do
   n_chars <- anyWord8
   Label <$> take (fromIntegral n_chars)


record_type :: Parser RecordType
record_type = RecordType <$> anyWord16


record_class :: Parser RecordClass
record_class = RecordClass <$> anyWord16


-- bytes arrive in network order,
-- so store the first byte first
anyWord16 :: Parser Word16
anyWord16 = do
    x <- anyWord8
    y <- anyWord8
    pure (pack16 x y)
