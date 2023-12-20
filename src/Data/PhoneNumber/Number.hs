{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE FieldSelectors #-}
#else
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif
-- | Data fields of t'PhoneNumber' objects.
module Data.PhoneNumber.Number
  ( PhoneNumber
  , pattern PhoneNumber
  , extension
  , rawInput
  , preferredDomesticCarrierCode
  , nationalNumber
  , countryCode
  , italianLeadingZero
  , countryCodeSource
  , numberOfLeadingZeros
  , CountryCode(..)
  , CountryCodeSource(..)
  ) where

import Data.ByteString (ByteString)
import Data.PhoneNumber.Internal.Number
  ( CPhoneNumber(CPhoneNumber), PhoneNumber, CountryCodeSource, toCPhoneNumber
  , fromCPhoneNumber
  )
import qualified Data.PhoneNumber.Internal.Number as I

-- | A country calling code (International Subscriber Dialing code, ISD code),
-- e.g. @34@ for Spain.
--
-- Contrary to the name, doesn't always correspond to a unique country (e.g.
-- @7@ could be either Russia or Kazakhstan), or a country at all, and instead a
-- non-geographical entity (e.g. @800@ is a Universal International Freephone
-- Service dialing code).
newtype CountryCode = CountryCode Int
  deriving newtype (Eq, Ord, Show, Num)

-- | Record pattern synonym for accessing data fields of the underlying C++
-- object. It can be used for record construction, record update, and record
-- pattern match. See t'PhoneNumber'.
pattern PhoneNumber
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Word
  -> CountryCode
  -> Maybe Bool
  -> Maybe CountryCodeSource
  -> Maybe Int
  -> PhoneNumber
pattern PhoneNumber
  { extension
  , rawInput
  , preferredDomesticCarrierCode
  , nationalNumber
  , countryCode
  , italianLeadingZero
  , countryCodeSource
  , numberOfLeadingZeros
  }
  <- (toCPhoneNumber -> CPhoneNumber
    { I.nationalNumber = fromIntegral -> nationalNumber
    , I.countryCode = fromIntegral -> CountryCode -> countryCode
    , I.numberOfLeadingZeros = fmap fromIntegral -> numberOfLeadingZeros
    , .. })
  where
    PhoneNumber extension rawInput preferredDomesticCarrierCode
      (fromIntegral -> nationalNumber)
      (CountryCode (fromIntegral -> countryCode)) italianLeadingZero
      countryCodeSource (fmap fromIntegral -> numberOfLeadingZeros)
        = fromCPhoneNumber $ CPhoneNumber{..}

{-# COMPLETE PhoneNumber #-}

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.Canonicalize' 'Nothing' "+1 800-234-5678 ext. 1234"
-- = 'Right' v'PhoneNumber' { 'extension' = 'Just' "1234", .. }
-- @
extension :: PhoneNumber -> Maybe ByteString

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.KeepRawInput' 'Nothing' " + 1(2-3~4*5.6 "
-- = 'Right' v'PhoneNumber' { 'rawInput' = 'Just' " + 1(2-3~4*5.6 ", .. }
-- @
rawInput :: PhoneNumber -> Maybe ByteString

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.KeepRawInput' ('Just' \"BR\") "0 41 (21) 2345-6789"
-- = 'Right' v'PhoneNumber' { 'preferredDomesticCarrierCode' = 'Just' "41", .. }
-- @
preferredDomesticCarrierCode :: PhoneNumber -> Maybe ByteString

-- | You probably want to use 'Data.PhoneNumber.Util.nationalSignificantNumber'
-- instead. E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.Canonicalize' 'Nothing' "+800 0001 2345"
-- = 'Right' v'PhoneNumber' { 'nationalNumber' = 12345, 'numberOfLeadingZeros' = Just 3, .. }
-- @
nationalNumber :: PhoneNumber -> Word

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.Canonicalize' ('Just' \"US\") "800-234-5678"
-- = 'Right' v'PhoneNumber' { 'countryCode' = 1, .. }
-- @
countryCode :: PhoneNumber -> CountryCode

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.Canonicalize' 'Nothing' "+39 06 1234 5678"
-- = 'Right' v'PhoneNumber' { 'italianLeadingZero' = 'Just' 'True', .. }
-- @
italianLeadingZero :: PhoneNumber -> Maybe Bool

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.KeepRawInput' ('Just' \"US\") "011 800 1234 5678"
-- = 'Right' v'PhoneNumber' { 'countryCodeSource' = 'Just' 'FromNumberWithIdd', .. }
-- @
countryCodeSource :: PhoneNumber -> Maybe CountryCodeSource

-- | E.g.:
--
-- @
-- 'Data.PhoneNumber.Util.parseNumber' 'Data.PhoneNumber.Util.Canonicalize' 'Nothing' "+800 0001 2345"
-- = 'Right' v'PhoneNumber' { 'numberOfLeadingZeros' = Just 3, 'nationalNumber' = 12345, .. }
-- @
numberOfLeadingZeros :: PhoneNumber -> Maybe Int
