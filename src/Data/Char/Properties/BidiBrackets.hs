module Data.Char.Properties.BidiBrackets (Type (..), paired) where

import Control.Monad (guard)
import Data.Bits
import Data.Bool
import Data.Int
import Data.Word

foreign import ccall unsafe "bidi_brackets" c_bidi_brackets :: Word32 -> Word32

data Type = O | C
  deriving (Eq, Read, Show, Enum, Bounded)

paired :: Char -> Maybe (Char, Type)
paired = \ x ->
    let x' = c_bidi_brackets (fromIntegral (fromEnum x))
        d = fromEnum $ shiftR (fromIntegral x' :: Int32) 29
    in (toEnum $ fromEnum x + d, bool O C $ testBit x' 28) <$ guard (d /= 0)
{-# INLINE paired #-}
