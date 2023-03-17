-- | Utilities for international phone numbers
module Data.PhoneNumber.Util
  ( -- * Parsing
    parseNumber
  , ParseMode(..)
  , ErrorType(..)
    -- * Formatting
  , formatNumber
  , PhoneNumberFormat(..)
    -- * Analysis
  , matchNumbers
  , MatchType(..)
  , regionForNumber
  , nationalSignificantNumber
  , isValidNumber
  , numberType
  , PhoneNumberType(..)
  , possibleNumber
  , ValidationResult(..)
  , canBeInternationallyDialed
  , isGeographicalNumber
  , isGeographicalNumberType
  , isAlphaNumber
    -- * Library Support
  , supportedRegions
  , supportedGlobalNetworkCallingCodes
  , supportedCallingCodes
  , supportedTypesForRegion
  , supportedTypesForNonGeoEntity
    -- * 'Region'/'CountryCode' Queries
  , Region(..)
  , noRegion
  , CountryCode(..)
  , countryCodeForRegion
  , regionForCountryCode
  , regionsForCountryCode
  , isNANPACountry
  , countryMobileToken
  , nddPrefixForRegion
    -- * Miscellaneous
  , truncateTooLongNumber
  , convertAlphaNumber
  , normalizeNumber
  , Normalize(..)
    -- * Re-exports
  , PhoneNumber
  )
  where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Data
import Data.PhoneNumber.Number
import Data.PhoneNumber.Internal.Util hiding (ErrorType(..), PhoneNumberFormat(..))
import qualified Data.PhoneNumber.Internal.Util as I
import qualified Data.Set as S
import GHC.Generics
import GHC.Exts
import GHC.IO

-- | Most of the time an ISO 3166-1 alpha-2 country code in upper case, except
-- in some cases the UN M.49 code @"001"@ (meaning the world) used to represent
-- non-geographical entities.
--
-- In some situations, the 'noRegion' value corresponds to a default or
-- unspecified region.
newtype Region = Region ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString, NFData)
  deriving stock (Data, Generic)

-- | A default or unspecified region, the user-assigned alpha-2 code @\"ZZ\"@
noRegion :: Region
noRegion = "ZZ"

-- | All geographical regions the library has metadata for
supportedRegions :: S.Set Region
supportedRegions = S.fromList $ unsafeDupablePerformIO $ mask_ $
  coerce c_phone_number_util_get_supported_regions

-- | All global network calling codes (country calling codes for
-- non-geographical entities) the library has metadata for
supportedGlobalNetworkCallingCodes :: S.Set CountryCode
supportedGlobalNetworkCallingCodes = S.fromList $ unsafeDupablePerformIO $ mask_ $
  map (CountryCode . fromIntegral) <$>
    c_phone_number_util_get_supported_global_network_calling_codes

-- | All country calling codes the library has metadata for, covering both
-- non-geographical entities (global network calling codes) and those used for
-- geographical entities. This could be used to populate a drop-down box of
-- country calling codes for a phone-number widget, for instance.
supportedCallingCodes :: S.Set CountryCode
supportedCallingCodes = S.fromList $ unsafeDupablePerformIO $ mask_ $
  map (CountryCode . fromIntegral) <$>
    c_phone_number_util_get_supported_calling_codes

-- | Returns the types for a given region which the library has metadata for.
-- Will not include 'FixedLineOrMobile' (if numbers for this non-geographical
-- entity could be classified as 'FixedLineOrMobile', both 'FixedLine' and
-- 'Mobile' would be present) and 'Unknown'.
--
-- No types will be returned for invalid or unknown region codes.
supportedTypesForRegion :: Region -> S.Set PhoneNumberType
supportedTypesForRegion (Region region) = S.fromList $ unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_get_supported_types_for_region region

-- | Returns the types for a country-code belonging to a non-geographical entity
-- which the library has metadata for. Will not include 'FixedLineOrMobile' (if
-- numbers for this non-geographical entity could be classified as
-- 'FixedLineOrMobile', both 'FixedLine' and 'Mobile' would be present) and
-- 'Unknown'.
--
-- No types will be returned for country calling codes that do not map to a
-- known non-geographical entity.
supportedTypesForNonGeoEntity :: CountryCode -> S.Set PhoneNumberType
supportedTypesForNonGeoEntity (CountryCode cc) = S.fromList $ unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_get_supported_types_for_non_geo_entity $ fromIntegral cc

-- | Returns true if the number is a valid vanity (alpha) number such as
-- @"800 MICROSOFT"@. A valid vanity number will start with at least 3 digits
-- and will have three or more alpha characters. This does not do
-- region-specific checks - to work out if this number is actually valid for a
-- region, you should use 'parseNumber' and 'possibleNumber'/'isValidNumber'.
isAlphaNumber :: ByteString -> Bool
isAlphaNumber bs = unsafeDupablePerformIO $
  c_phone_number_util_is_alpha_number bs

-- | Converts all alpha characters in a number to their respective digits on
-- a keypad, but retains existing formatting
convertAlphaNumber :: ByteString -> ByteString
convertAlphaNumber bs = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_convert_alpha_characters_in_number bs

-- | How 'normalizeNumber' should normalize a phone number
data Normalize
  = Digits -- ^ Convert wide-ascii and arabic-indic numerals to European
      -- numerals, and strip punctuation and alpha characters
  | Dialable -- ^ Strip all characters which are not diallable on a mobile phone
      -- keypad (including all non-ASCII digits)
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving anyclass (NFData)

-- | Normalizes a string of characters representing a phone number. See
-- 'Normalize'.
normalizeNumber :: Normalize -> ByteString -> ByteString
normalizeNumber Digits bs = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_normalize_digits_only bs
normalizeNumber Dialable bs = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_normalize_dialable_chars_only bs

-- | Gets the National Significant Number (NSN) of a phone number. Note an NSN
-- doesn't contain a national prefix or any formatting.
nationalSignificantNumber :: PhoneNumber -> ByteString
nationalSignificantNumber pn = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_get_national_significant_number pn

-- | Returns the mobile token for the provided country calling code if it has
-- one, otherwise returns an empty string. A mobile token is a number inserted
-- before the area code when dialing a mobile number from that country from
-- abroad.
countryMobileToken :: CountryCode -> ByteString
countryMobileToken (CountryCode cc) = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_get_country_mobile_token $ fromIntegral cc

-- | How 'formatNumber' should format a phone number
data PhoneNumberFormat
  = International -- ^ Consistent with the definition in ITU-T Recommendation
      -- E.123. However we follow local conventions such as using @\'-\'@
      -- instead of whitespace as separators. E.g. @"+41 44 668 1800"@.
  | National -- ^ Consistent with E.123, and also following local conventions
      -- for separators. E.g. @"044 668 1800"@.
  | E164 -- ^ Same as 'International' but with no formatting, e.g.
      -- @"+41446681800"@. See https://en.wikipedia.org/wiki/E.164.
  | RFC3966 -- ^ Same as 'International' but with all separating symbols
      -- replaced with a hyphen, and with any phone number extension appended
      -- with @";ext="@. It will also have a prefix of @"tel:"@ added, e.g.
      -- @"tel:+41-44-668-1800"@.
  | NationalWithCarrierCodeOverride ByteString -- ^ Same as 'National' but
      -- for dialing using the specified domestic carrier code.
  | NationalWithCarrierCodeFallback ByteString -- ^ Same as 'National' but
      -- use the phone number's 'preferredDomesticCarrierCode' (which is only
      -- set if parsed with 'KeepRawInput'). If a preferred carrier code is
      -- absent, the provided string is used as fallback.
  | ForMobileDialing -- ^ Format in such a way that it can be dialed from a
      -- mobile phone in a specific region. If the number cannot be reached from
      -- the region (e.g. some countries block toll-free numbers from being
      -- called outside of the country), will format to an empty string.
    { from :: Region
    , withFormatting :: Bool -- ^ Whether to strip formatting as in 'E164'.
    }
  | OutOfCountry -- ^ Format for out-of-country dialing purposes. This takes
      -- care of the case of calling inside of NANPA and between Russia and
      -- Kazakhstan (who share the same country calling code). In those cases,
      -- no international prefix is used. For regions which have multiple
      -- international prefixes, formats as 'International'.
    { from :: Region
    , keepAlphaChars :: Bool -- ^ Attempt to keep alpha chars and grouping
      -- information, if 'rawInput' is available. Setting this to 'True' comes
      -- with a number of caveats:
      --
      -- 1. This will not produce good results if the country calling code is
      -- both present in 'rawInput' /and/ is the start of the national number.
      -- This is not a problem in the regions which typically use alpha numbers.
      --
      -- 2. This will also not produce good results if 'rawInput' has any
      -- grouping information within the first three digits of the national
      -- number, and if the function needs to strip preceding digits/words in
      -- 'rawInput' before these digits. Normally people group the first three
      -- digits together so this is not a huge problem.
    }
  | Original -- ^ Use 'rawInput' verbatim if present, otherwise infer
      -- 'National', 'International', or 'OutOfCountry' based on
      -- 'countryCodeSource'.
    { from :: Region
    }

-- | Formats a phone number in the specified 'PhoneNumberFormat' using default
-- rules. Note that this does not promise to produce a phone number that the
-- user can dial from where they are - as we do not currently support a more
-- abbreviated format, such as for users in the same area who could potentially
-- dial the number without area code.
formatNumber :: PhoneNumberFormat -> PhoneNumber -> ByteString
formatNumber fmt pn = case fmt of
  E164 -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format pn I.E164
  International -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format pn I.International
  National -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format pn I.National
  RFC3966 -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format pn I.Rfc3966
  NationalWithCarrierCodeOverride cc -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format_national_number_with_carrier_code pn cc
  NationalWithCarrierCodeFallback cc -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format_national_number_with_preferred_carrier_code pn cc
  ForMobileDialing (Region region) keepFmt -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format_number_for_mobile_dialing pn region keepFmt
  Original (Region region) -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format_in_original_format pn region
  OutOfCountry (Region region) False -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format_out_of_country_calling_number pn region
  OutOfCountry (Region region) True -> unsafeDupablePerformIO $ mask_ $
    c_phone_number_util_format_out_of_country_keeping_alpha_chars pn region

-- | Attempts to extract a valid number from a phone number that is too long to
-- be valid. Returns 'Nothing' if no valid number could be extracted.
truncateTooLongNumber :: PhoneNumber -> Maybe PhoneNumber
truncateTooLongNumber pn = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_truncate_too_long_number pn

-- | Gets the phone number type. Returns 'Unknown' if invalid.
numberType :: PhoneNumber -> PhoneNumberType
numberType pn = unsafeDupablePerformIO $
  c_phone_number_util_get_number_type pn

-- | Tests whether a phone number is valid for a certain region (if unspecified,
-- the region the number is from). Note this doesn't verify the number is
-- actually in use, which is impossible to tell by just looking at a number
-- itself.
--
-- If the country calling code is not the same as the country calling code for
-- the provided region, this immediately returns 'False'. After this, the
-- specific number pattern rules for the region are examined.
--
-- Specifying a region may be useful for determining for example whether a
-- particular number is valid for Canada, rather than just a valid NANPA number.
-- On the other hand this may lead to undesirable results, for example numbers
-- from British Crown dependencies such as the Isle of Man are considered
-- invalid for the region @\"GB\"@ (United Kingdom), since it has its own region
-- code, @\"IM\"@.
--
-- Note that it only verifies whether the parsed, canonicalised number is valid:
-- not whether a particular series of digits entered by the user is dialable
-- from the region provided when parsing. For example, the number
-- @+41 (0) 78 927 2696@ can be parsed into a number with country code @"41"@
-- and National Significant Number @"789272696"@. This is valid, while the
-- original string is not dialable.
isValidNumber :: Maybe Region -> PhoneNumber -> Bool
isValidNumber Nothing pn = unsafeDupablePerformIO $
  c_phone_number_util_is_valid_number pn
isValidNumber (Just (Region region)) pn = unsafeDupablePerformIO $
  c_phone_number_util_is_valid_number_for_region pn region

-- | Returns the region where a phone number is from. This could be used for
-- geocoding at the region level. Only guarantees correct results for valid,
-- full numbers (not short-codes, or invalid numbers).
regionForNumber :: PhoneNumber -> Region
regionForNumber pn = unsafeDupablePerformIO $ mask_ $
  coerce $ c_phone_number_util_get_region_code_for_number pn

-- | Returns the country calling code for a specific region. For example, this
-- would be @1@ for the United States, and @64@ for New Zealand.
countryCodeForRegion :: Region -> CountryCode
countryCodeForRegion (Region region) = unsafeDupablePerformIO $
  CountryCode . fromIntegral <$>
    c_phone_number_util_get_country_code_for_region region

-- | Returns the region code that matches the specific country code. Note that
-- it is possible that several regions share the same country calling code
-- (e.g. US and Canada), and in that case, only one of the regions (normally the
-- one with the largest population) is returned. If the country calling code
-- entered is valid but doesn't match a specific region (such as in the case of
-- non-geographical calling codes like @800@) the @'Region' "001"@ will be
-- returned.
regionForCountryCode :: CountryCode -> Region
regionForCountryCode (CountryCode cc) = unsafeDupablePerformIO $ mask_ $
  coerce $ c_phone_number_util_get_region_code_for_country_code $ fromIntegral cc

-- | Returns a list of the region codes that match the specific country calling
-- code. For non-geographical country calling codes, the region code @"001"@ is
-- returned. Also, in the case of no region code being found, the list is empty.
regionsForCountryCode :: CountryCode -> [Region]
regionsForCountryCode (CountryCode cc) = unsafeDupablePerformIO $ mask_ $
  coerce $ c_phone_number_util_get_region_codes_for_country_calling_code $ fromIntegral cc

-- | Checks if this is a region under the North American Numbering Plan
-- Administration (NANPA).
isNANPACountry :: Region -> Bool
isNANPACountry (Region region) = unsafeDupablePerformIO $
  c_phone_number_util_is_nanpa_country region

-- | Returns the National Direct Dialling prefix (NDD prefix) for a specific
-- region. For example, this would be @"1"@ for the United States, and @"0"@ for
-- New Zealand. Note that this may contain symbols like @\'~\'@ (which indicates
-- a wait for a dialing tone). Returns an empty string if no national prefix is
-- present.
nddPrefixForRegion
  :: Bool -- ^ Whether to strip non-digits like @\'~\'@
  -> Region
  -> ByteString
nddPrefixForRegion strip (Region region) = unsafeDupablePerformIO $ mask_ $
  c_phone_number_util_get_ndd_prefix_for_region region strip

-- | Check whether a phone number is a possible number of a particular type.
-- Pass the type 'Unknown' to check whether a number is possible at all.
--
-- For more specific types that don't exist in a particular region, this will
-- return a result that isn't so useful; it is recommended that you use
-- 'supportedTypesForRegion' or 'supportedTypesForNonGeoEntity' respectively
-- before calling this function to determine you should pass a more specific
-- type instead of 'Unknown'.
--
-- This function provides a more lenient check than 'isValidNumber' in the
-- following sense:
--
-- 1. It only checks the length of phone numbers. In particular, it doesn't
-- check starting digits of the number.
--
-- 2. If 'Unknown' is provided, it doesn't attempt to figure out the type of the
-- number, but uses general rules which apply to all types of phone numbers in a
-- region. Therefore, it is much faster than 'isValidNumber'.
--
-- 3. For some numbers (particularly fixed-line), many regions have the concept
-- of area code, which together with subscriber number constitute the National
-- Significant Number. It is sometimes okay to dial only the subscriber number
-- when dialing in the same area. This function will return IsPossibleLocalOnly
-- if the subscriber-number-only version is passed in. On the other hand,
-- because 'isValidNumber' validates using information on both starting digits
-- (for fixed line numbers, that would most likely be area codes) and length
-- (obviously includes the length of area codes for fixed line numbers), it will
-- return 'False' for the subscriber-number-only version.
possibleNumber :: PhoneNumberType -> PhoneNumber -> ValidationResult
possibleNumber ntype pn = unsafeDupablePerformIO $
  c_phone_number_util_is_possible_number_for_type_with_reason pn ntype

-- | Returns 'True' if the number can be dialed from outside the region, or
-- unknown. If the number can only be dialled from within the region, returns
-- 'False'. Does not check the number is a valid number. Note that, at the
-- moment, this method does not handle short numbers (which are currently all
-- presumed to not be diallable from outside their country).
canBeInternationallyDialed :: PhoneNumber -> Bool
canBeInternationallyDialed pn = unsafeDupablePerformIO $
  c_phone_number_util_can_be_internationally_dialed pn

-- | Tests whether a phone number has a geographical association. It checks if
-- the number is associated with a certain region in the country to which it
-- belongs. Note that this doesn't verify if the number is actually in use.
isGeographicalNumber :: PhoneNumber -> Bool
isGeographicalNumber pn = unsafeDupablePerformIO $
  c_phone_number_util_is_number_geographical_1 pn

-- | A less expensive version of 'isGeographicalNumber' if we already know the
-- 'PhoneNumberType'
isGeographicalNumberType :: PhoneNumberType -> CountryCode -> Bool
isGeographicalNumberType ntype (CountryCode cc) = unsafeDupablePerformIO $
  c_phone_number_util_is_number_geographical_2 ntype $ fromIntegral cc

-- | Phone number parsing error
data ErrorType
  = InvalidCountryCodeError -- ^ The number did not contain a country code and
    -- there was no default region supplied, or the number contained an invalid
    -- country code
  | NotANumber -- ^ Does not look like a phone number
  | TooShortAfterIdd -- ^ Input starts with an International Direct Dialing
    -- prefix, but ends too shortly thereafter
  | TooShortNsn -- ^ The National Significant Number is too short
  | TooLongNsn -- ^ The National Significant Number is too long
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving anyclass (NFData)

-- | How much information to retain when parsing
data ParseMode
  = Canonicalize -- ^ Canonicalize the phone number such that different
      -- representations can be easily compared, no matter what form it was
      -- originally entered in (e.g. national, international)
  | KeepRawInput -- ^ Record context about the number being parsed, such as
      -- 'rawInput', 'countryCodeSource', and 'preferredDomesticCarrierCode'
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving anyclass (NFData)

-- | Parse a phone number.
--
-- The function is quite lenient and looks for a number in the input
-- 'ByteString' and does not check whether the string is definitely only a phone
-- number. To do this, it ignores punctuation and white-space, as well as any
-- text before the number (e.g. a leading @"Tel: "@) and trims the non-number
-- bits. It will accept a number in any format (E164, national, international
-- etc.), assuming it can be interpreted with the default t'Region' supplied. It
-- also attempts to convert any alpha characters into digits if it thinks this
-- is a vanity number of the type @"1800 MICROSOFT"@.
--
-- The input can contain formatting such as @+@, @(@ and @-@, as well as a phone
-- number extension. It can also be provided in RFC3966 format.
--
-- Note that validation of whether the number is actually a valid number for a
-- particular region is not performed. This can be done separately with
-- 'isValidNumber'.
--
-- Returns an error if the string is not considered to be a viable phone number
-- (e.g. too few or too many digits) or if 'noRegion' was supplied and the
-- number is not in international format (does not start with @\'+\'@).
parseNumber
  :: ParseMode
  -> Region -- ^ Default region, the country that we are expecting the number to
    -- be dialed from, which affects national and international dialing
    -- prefixes. This is only used if the number being parsed is not written in
    -- international format. In such cases the 'countryCode' of the number would
    -- be that of the default region supplied. If the number is guaranteed to
    -- start with a @\'+\'@ followed by the country calling code, then
    -- 'noRegion' can be supplied.
  -> ByteString -- ^ Input.
  -> Either ErrorType PhoneNumber
parseNumber mode (Region region) number = case err of
  I.NoParsingError -> Right pn
  I.InvalidCountryCodeError -> Left InvalidCountryCodeError
  I.NotANumber -> Left NotANumber
  I.TooShortAfterIdd -> Left TooShortAfterIdd
  I.TooShortNsn -> Left TooShortNsn
  I.TooLongNsn -> Left TooLongNsn
  where
    (err, pn) = case mode of
      Canonicalize -> unsafeDupablePerformIO $ mask_ $
        c_phone_number_util_parse number region
      KeepRawInput -> unsafeDupablePerformIO $ mask_ $
        c_phone_number_util_parse_and_keep_raw_input number region

-- | Compares two numbers for equality. A number can be provided as a string, in
-- which case it is parsed without assuming its region.
--
-- Returns 'ExactMatch' if the country calling code, National Significant
-- Number (NSN), presence of a leading zero for Italian numbers and any
-- extension present are the same.
--
-- Returns 'NsnMatch' if either or both has no country calling code specified,
-- and the NSNs and extensions are the same.
--
-- Returns 'ShortNsnMatch' if either or both has no country calling code
-- specified, or the country calling code specified is the same, and one NSN
-- could be a shorter version of the other number. This includes the case where
-- one has an extension specified, and the other does not.
--
-- Returns 'InvalidNumber' if a number that was provided as a string could not
-- be parsed.
--
-- Returns 'NoMatch' otherwise.
--
-- For example, the numbers @1 345 657 1234@ and @657 1234@ are a
-- 'ShortNsnMatch'. The numbers @1 345 657 1234@ and @345 657@ are a 'NoMatch'.
-- Note that none of these numbers can be parsed by 'parseNumber' without
-- assuming a region.
matchNumbers :: Either ByteString PhoneNumber -> Either ByteString PhoneNumber -> MatchType
matchNumbers (Left pn1) (Left pn2) = unsafeDupablePerformIO $
  c_phone_number_util_is_number_match_with_two_strings pn1 pn2
matchNumbers (Left pn1) (Right pn2) = unsafeDupablePerformIO $
  c_phone_number_util_is_number_match_with_one_string pn2 pn1
matchNumbers (Right pn1) (Left pn2) = unsafeDupablePerformIO $
  c_phone_number_util_is_number_match_with_one_string pn1 pn2
matchNumbers (Right pn1) (Right pn2) = unsafeDupablePerformIO $
  c_phone_number_util_is_number_match pn1 pn2
