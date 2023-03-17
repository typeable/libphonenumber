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
-- object. If on GHC 9.2+, it can be used for record construction, record
-- update, and record pattern match. See t'PhoneNumber'.
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
#if __GLASGOW_HASKELL__ >= 902
  { extension
  , rawInput
  , preferredDomesticCarrierCode
  , nationalNumber
  , countryCode
  , italianLeadingZero
  , countryCodeSource
  , numberOfLeadingZeros
  }
#else
  extension rawInput preferredDomesticCarrierCode nationalNumber countryCode
  italianLeadingZero countryCodeSource numberOfLeadingZeros
#endif
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

#if __GLASGOW_HASKELL__ < 902
extension :: PhoneNumber -> Maybe ByteString
extension (PhoneNumber x _ _ _ _ _ _ _) = x

rawInput :: PhoneNumber -> Maybe ByteString
rawInput (PhoneNumber _ x _ _ _ _ _ _) = x

preferredDomesticCarrierCode :: PhoneNumber -> Maybe ByteString
preferredDomesticCarrierCode (PhoneNumber _ _ x _ _ _ _ _) = x

nationalNumber :: PhoneNumber -> Word
nationalNumber (PhoneNumber _ _ _ x _ _ _ _) = x

countryCode :: PhoneNumber -> CountryCode
countryCode (PhoneNumber _ _ _ _ x _ _ _) = x

italianLeadingZero :: PhoneNumber -> Maybe Bool
italianLeadingZero (PhoneNumber _ _ _ _ _ x _ _) = x

countryCodeSource :: PhoneNumber -> Maybe CountryCodeSource
countryCodeSource (PhoneNumber _ _ _ _ _ _ x _) = x

numberOfLeadingZeros :: PhoneNumber -> Maybe Int
numberOfLeadingZeros (PhoneNumber _ _ _ _ _ _ _ x) = x
#endif

{-# COMPLETE PhoneNumber #-}
