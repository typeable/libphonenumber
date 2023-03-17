module Data.PhoneNumber.Internal.Common where

import Control.Arrow
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Foreign
import Foreign.C.Types

#include <stddef.h>

alloca2 :: (Storable a, Storable b) => ((Ptr a, Ptr b) -> IO c) -> IO c
alloca2 k = alloca $ \p1 -> alloca $ \p2 -> k (p1, p2)

withByteString :: ByteString -> ((Ptr CChar, {#type size_t#}) -> IO a) -> IO a
withByteString bs k = unsafeUseAsCStringLen bs $ k . second fromIntegral

-- Any #fun using acquireCString must be wrapped in a mask, so that an exception
-- cannot arrive before we assign a finalizer to the string
acquireCString :: Ptr CChar -> {#type size_t#} -> IO ByteString
acquireCString = curry $ unsafePackMallocCStringLen . second fromIntegral
