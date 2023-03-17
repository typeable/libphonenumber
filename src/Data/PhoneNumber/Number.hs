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
import Data.PhoneNumber.Internal.Number hiding
  ( pattern PhoneNumber
  , extension
  , rawInput
  , preferredDomesticCarrierCode
  , nationalNumber
  , countryCode
  , italianLeadingZero
  , countryCodeSource
  , numberOfLeadingZeros )
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
