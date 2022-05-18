-- functions for packing and unpacking
-- various sizes of Word,
-- using network byte-order.
module HSDNS.Common.Packing (
    pack16,
    unpack16,
    pack32,
    unpack32,
) where

import Data.Word (Word8, Word16, Word32)
import Data.Bits (shift)


pack16 :: Word8 -> Word8 -> Word16
pack16 x y = x16 + y16
    where
        x16 = fromIntegral x `shift` 8
        y16 = fromIntegral y

unpack16 :: Word16 -> (Word8, Word8)
-- unpack_16 = x `divMod` 256   -- a little too cute
unpack16 x = (a8, b8)
    where
        a8 = fromIntegral (x `shift` (-8))
        b8 = fromIntegral x

pack32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
pack32 w x y z = w32 + x32 + y32 + z32
    where
        w32 = (fromIntegral w) `shift` 24
        x32 = (fromIntegral x) `shift` 16
        y32 = (fromIntegral y) `shift` 8
        z32 = fromIntegral z

unpack32 :: Word32 -> (Word8, Word8, Word8, Word8)
unpack32 x = (a8, b8, c8, d8)
    where
        a8 = fromIntegral (x `shift` (-24))
        b8 = fromIntegral (x `shift` (-16))
        c8 = fromIntegral (x `shift` (-8))
        d8 = fromIntegral x
