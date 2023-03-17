{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.PhoneNumber.Internal.Util where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.PhoneNumber.Internal.Common
{#import Data.PhoneNumber.Internal.Number#}
import Foreign
import Foreign.C.Types
import GHC.Generics (Generic)

#include <stdlib.h>
#include "c_phonenumberutil.h"

newtype CString = CString (Ptr CChar, {#type size_t#})

unCString :: CString -> (Ptr CChar, {#type size_t#})
unCString (CString pair) = pair

instance Storable CString where
  sizeOf _ = {#sizeof c_string#}
  alignment _ = {#alignof c_string#}
  peek p = CString <$> liftA2 (,)
    ({#get c_string->string#} p)
    ({#get c_string->string_size#} p)
  poke p (CString (str, sz)) = do
    {#set c_string->string#} p str
    {#set c_string->string_size#} p sz

{#pointer *c_string foreign -> CString nocode#}

{#enum phone_number_format as PhoneNumberFormat {underscoreToCase}#}

-- | Type of a phone number.
--
-- 'FixedLineOrMobile' designates cases where it is impossible to distinguish
-- between fixed-line and mobile numbers by looking at the phone number itself
-- (e.g. the USA).
--
-- 'TollFree' designates freephone lines.
--
-- 'SharedCost' designates numbers where the cost of the call is shared between
-- the caller and the recipient, and is hence typically less than for
-- 'PremiumRate'. See http://en.wikipedia.org/wiki/Shared_Cost_Service for more
-- information.
--
-- 'Voip' designates Voice over IP numbers. This includes TSoIP (Telephony
-- Service over IP).
--
-- 'Uan' designates "Universal Access Numbers" or "Company Numbers". They may be
-- further routed to specific offices, but allow one number to be used for a
-- company.
--
-- 'Voicemail' designates "Voice Mail Access Numbers".
{#enum phone_number_type as PhoneNumberType {underscoreToCase}
  deriving (Eq, Ord, Show, Generic)#}

-- | Types of phone number matches. See 'Data.PhoneNumber.Util.matchNumbers'.
{#enum match_type as MatchType {underscoreToCase}
  deriving (Eq, Show, Generic)#}

{#enum error_type as ErrorType {underscoreToCase}#}

-- | Possible outcomes when testing if a t'PhoneNumber' is possible.
--
-- 'IsPossible' means the number length matches that of valid numbers for this
-- region.
--
-- 'IsPossibleLocalOnly' means the number length matches that of local numbers
-- for this region only (i.e. numbers that may be able to be dialled within an
-- area, but do not have all the information to be dialled from anywhere inside
-- or outside the country).
--
-- 'InvalidCountryCode' means the number has an invalid country calling code.
--
-- 'TooShort' means the number is shorter than all valid numbers for this
-- region.
--
-- 'TooLong' means the number is longer than all valid numbers for this region.
--
-- 'InvalidLength' means the number is longer than the shortest valid numbers
-- for this region, shorter than the longest valid numbers for this region, and
-- does not itself have a number length that matches valid numbers for this
-- region. This can also be returned in the case when there are no numbers of a
-- specific type at all for this region.
{#enum validation_result as ValidationResult {underscoreToCase}
  deriving (Eq, Show, Generic)#}

{#fun unsafe c_phone_number_util_get_supported_regions
  { alloca2- `[ByteString]'& peekAcquireCStringList*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_supported_global_network_calling_codes
  { alloca2- `[CInt]'& peekAcquireList*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_supported_calling_codes
  { alloca2- `[CInt]'& peekAcquireList*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_supported_types_for_region
  { withByteString* `ByteString'&
  , alloca2- `[PhoneNumberType]'& peekAcquireEnumList*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_supported_types_for_non_geo_entity
  { `CInt'
  , alloca2- `[PhoneNumberType]'& peekAcquireEnumList*
  } -> `()'#}

{#fun unsafe c_phone_number_util_is_alpha_number
  { withByteString* `ByteString'&
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_convert_alpha_characters_in_number
  { withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_normalize_digits_only
  { withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_normalize_dialable_chars_only
  { withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_national_significant_number
  { `PhoneNumber'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}


{#fun unsafe c_phone_number_util_get_country_mobile_token
  { `CInt'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format
  { `PhoneNumber'
  , `PhoneNumberFormat'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format_national_number_with_carrier_code
  { `PhoneNumber'
  , withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format_national_number_with_preferred_carrier_code
  { `PhoneNumber'
  , withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format_number_for_mobile_dialing
  { `PhoneNumber'
  , withByteString* `ByteString'&
  , `Bool'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format_out_of_country_calling_number
  { `PhoneNumber'
  , withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format_in_original_format
  { `PhoneNumber'
  , withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_format_out_of_country_keeping_alpha_chars
  { `PhoneNumber'
  , withByteString* `ByteString'&
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_truncate_too_long_number
  { `PhoneNumber'
  } -> `Maybe PhoneNumber' acquireMaybePhoneNumber*#}
  where
    acquireMaybePhoneNumber p
      | p == nullPtr = pure Nothing
      | otherwise = Just <$> acquirePhoneNumber p

{#fun unsafe c_phone_number_util_get_number_type
  { `PhoneNumber'
  } -> `PhoneNumberType'#}

{#fun unsafe c_phone_number_util_is_valid_number
  { `PhoneNumber'
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_is_valid_number_for_region
  { `PhoneNumber'
  , withByteString* `ByteString'&
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_get_region_code_for_number
  { `PhoneNumber'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_country_code_for_region
  { withByteString* `ByteString'&
  } -> `CInt'#}

{#fun unsafe c_phone_number_util_get_region_code_for_country_code
  { `Int'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_get_region_codes_for_country_calling_code
  { `Int'
  , alloca2- `[ByteString]'& peekAcquireCStringList*
  } -> `()'#}

{#fun unsafe c_phone_number_util_is_nanpa_country
  { withByteString* `ByteString'&
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_get_ndd_prefix_for_region
  { withByteString* `ByteString'&
  , `Bool'
  , alloca- `ByteString' peekAcquireCString*
  } -> `()'#}

{#fun unsafe c_phone_number_util_is_possible_number_for_type_with_reason
  { `PhoneNumber'
  , `PhoneNumberType'
  } -> `ValidationResult'#}

{#fun unsafe c_phone_number_util_can_be_internationally_dialed
  { `PhoneNumber'
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_is_number_geographical_1
  { `PhoneNumber'
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_is_number_geographical_2
  { `PhoneNumberType'
  , `CInt'
  } -> `Bool'#}

{#fun unsafe c_phone_number_util_parse
  { withByteString* `ByteString'&
  , withByteString* `ByteString'&
  , alloca- `PhoneNumber' peekAcquirePhoneNumber*
  } -> `ErrorType'#}

{#fun unsafe c_phone_number_util_parse_and_keep_raw_input
  { withByteString* `ByteString'&
  , withByteString* `ByteString'&
  , alloca- `PhoneNumber' peekAcquirePhoneNumber*
  } -> `ErrorType'#}

{#fun unsafe c_phone_number_util_is_number_match
  { `PhoneNumber'
  , `PhoneNumber'
  } -> `MatchType'#}

{#fun unsafe c_phone_number_util_is_number_match_with_two_strings
  { withByteString* `ByteString'&
  , withByteString* `ByteString'&
  } -> `MatchType'#}

{#fun unsafe c_phone_number_util_is_number_match_with_one_string
  { `PhoneNumber'
  , withByteString* `ByteString'&
  } -> `MatchType'#}

peekAcquireList :: Storable a => Ptr (Ptr a) -> Ptr {#type size_t#} -> IO [a]
peekAcquireList pList pSize = do
  size <- peek pSize
  list <- peek pList
  xs <- peekArray (fromIntegral size) list
  {#call unsafe free as stdlibFree#} $ castPtr list
  pure xs

peekAcquireCStringList :: Ptr (Ptr CString) -> Ptr {#type size_t#} -> IO [ByteString]
peekAcquireCStringList pList pSize =
  traverse (uncurry acquireCString . unCString) =<< peekAcquireList pList pSize

peekAcquireEnumList :: Enum a => Ptr (Ptr CInt) -> Ptr {#type size_t#} -> IO [a]
peekAcquireEnumList pList pSize =
  map (toEnum . fromIntegral) <$> peekAcquireList pList pSize

peekAcquireCString :: Ptr CString -> IO ByteString
peekAcquireCString p = uncurry acquireCString . unCString =<< peek p

acquirePhoneNumber :: Ptr PhoneNumber -> IO PhoneNumber
acquirePhoneNumber p = PhoneNumber <$> newForeignPtr c_phone_number_free p

peekAcquirePhoneNumber :: Ptr (Ptr PhoneNumber) -> IO PhoneNumber
peekAcquirePhoneNumber p = acquirePhoneNumber =<< peek p
