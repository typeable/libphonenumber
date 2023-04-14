module Data.PhoneNumber.Internal.Number where

import Control.DeepSeq
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.ByteString (ByteString)
import Data.Data
import Data.PhoneNumber.Internal.Common
import Data.List
import Foreign
import GHC.Generics (Generic)
import GHC.IO
import qualified GHC.Read as P
import qualified Text.ParserCombinators.ReadPrec as P
import qualified Text.Read.Lex as P

#include "c_phonenumber.h"

{#pointer *phone_number foreign -> CPhoneNumber nocode#}

data CPhoneNumber = CPhoneNumber
  { extension :: !(Maybe ByteString)
  , rawInput :: !(Maybe ByteString)
  , preferredDomesticCarrierCode :: !(Maybe ByteString)
  , nationalNumber :: !{#type uint64_t#}
  , countryCode :: !{#type int32_t#}
  , italianLeadingZero :: !(Maybe Bool)
  , countryCodeSource :: !(Maybe CountryCodeSource)
  , numberOfLeadingZeros :: !(Maybe {#type int32_t#})
  }
  deriving (Eq, Ord, Show, Read)

-- | Indicates what information was used to fill the
-- 'Data.PhoneNumber.Number.countryCode' field of t'PhoneNumber'.
{#enum country_code_source as CountryCodeSource {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Data, Generic)#}

deriving anyclass instance NFData CountryCodeSource

withCPhoneNumber :: CPhoneNumber -> (Ptr CPhoneNumber -> IO a) -> IO a
withCPhoneNumber CPhoneNumber{..} = runContT $ do
  p <- ContT $ allocaBytes {#sizeof phone_number#}
  withMaybeCString extension
    ({#set phone_number->extension#} p)
    ({#set phone_number->extension_size#} p)
  withMaybeCString rawInput
    ({#set phone_number->raw_input#} p)
    ({#set phone_number->raw_input_size#} p)
  withMaybeCString preferredDomesticCarrierCode
    ({#set phone_number->preferred_domestic_carrier_code#} p)
    ({#set phone_number->preferred_domestic_carrier_code_size#} p)
  lift $ {#set phone_number->national_number#} p nationalNumber
  lift $ {#set phone_number->country_code#} p countryCode
  lift $ {#set phone_number->has_italian_leading_zero#} p
    =<< case italianLeadingZero of
      Nothing -> pure 0
      Just ilz -> 1 <$ {#set phone_number->italian_leading_zero#} p (fromIntegral $ fromEnum ilz)
  lift $ {#set phone_number->has_country_code_source#} p
    =<< case countryCodeSource of
      Nothing -> pure 0
      Just ccs -> 1 <$ {#set phone_number->country_code_source#} p (fromIntegral $ fromEnum ccs)
  lift $ {#set phone_number->has_number_of_leading_zeros#} p
    =<< case numberOfLeadingZeros of
      Nothing -> pure 0
      Just nlz -> 1 <$ {#set phone_number->number_of_leading_zeros#} p (fromIntegral $ fromEnum nlz)
  pure p
  where
    withMaybeCString Nothing setP _ = do
      lift $ setP nullPtr
    withMaybeCString (Just bs) setP setSZ = do
      (p, sz) <- ContT $ withByteString bs
      () <- lift $ setP p
      lift $ setSZ sz

-- Any #fun using acquireCPhoneNumber must be wrapped in a mask, so that an
-- exception cannot arrive before we assign finalizers to the strings inside
acquireCPhoneNumber :: Ptr CPhoneNumber -> IO CPhoneNumber
acquireCPhoneNumber p = do
  extension <- acquireMaybeCString
    ({#get phone_number->extension#} p)
    ({#get phone_number->extension_size#} p)
  rawInput <- acquireMaybeCString
    ({#get phone_number->raw_input#} p)
    ({#get phone_number->raw_input_size#} p)
  preferredDomesticCarrierCode <- acquireMaybeCString
    ({#get phone_number->preferred_domestic_carrier_code#} p)
    ({#get phone_number->preferred_domestic_carrier_code_size#} p)
  nationalNumber <- {#get phone_number->national_number#} p
  countryCode <- {#get phone_number->country_code#} p
  italianLeadingZero <- {#get phone_number->has_italian_leading_zero#} p
    >>= \case
      0 -> pure Nothing
      _ -> Just . toEnum . fromIntegral <$> {#get phone_number->italian_leading_zero#} p
  countryCodeSource <- {#get phone_number->has_country_code_source#} p
    >>= \case
      0 -> pure Nothing
      _ -> Just . toEnum . fromIntegral <$> {#get phone_number->country_code_source#} p
  numberOfLeadingZeros <- {#get phone_number->has_number_of_leading_zeros#} p
    >>= \case
      0 -> pure Nothing
      _ -> Just <$> {#get phone_number->number_of_leading_zeros#} p
  pure $ CPhoneNumber{..}
  where
    acquireMaybeCString getP getSZ = do
      string <- getP
      if string == nullPtr
      then pure Nothing
      else Just <$> (acquireCString string =<< getSZ)

-- Any #fun returning PhoneNumber must be wrapped in a mask, so that an
-- exception cannot arrive before we assign a finalizer
-- | A decoded phone number. While internally it is a handle for the
-- corresponding C++ object, for most intents and purposes it can be used as
-- a record (using the v'Data.PhoneNumber.Number.PhoneNumber' record pattern
-- synonym) with the following structure:
--
-- @
-- v'Data.PhoneNumber.Number.PhoneNumber'
-- { 'Data.PhoneNumber.Number.extension' :: !('Maybe' 'ByteString')
-- , 'Data.PhoneNumber.Number.rawInput' :: !('Maybe' 'ByteString')
-- , 'Data.PhoneNumber.Number.preferredDomesticCarrierCode' :: !('Maybe' 'ByteString')
-- , 'Data.PhoneNumber.Number.nationalNumber' :: !'Word'
-- , 'Data.PhoneNumber.Number.countryCode' :: ! t'Data.PhoneNumber.Number.CountryCode'
-- , 'Data.PhoneNumber.Number.italianLeadingZero' :: !('Maybe' 'Bool')
-- , 'Data.PhoneNumber.Number.countryCodeSource' :: !('Maybe' 'CountryCodeSource')
-- , 'Data.PhoneNumber.Number.numberOfLeadingZeros' :: !('Maybe' 'Int')
-- }
-- @
{#pointer *CxxPhoneNumber as PhoneNumber foreign finalizer c_phone_number_free newtype#}

{#fun unsafe c_phone_number_marshal
  { withCPhoneNumber* `CPhoneNumber'
  } -> `PhoneNumber'#}

{#fun unsafe c_phone_number_unmarshal
  { `PhoneNumber'
  , allocaCPhoneNumber- `CPhoneNumber' acquireCPhoneNumber*
  } -> `()' #}
  where
    allocaCPhoneNumber :: (Ptr CPhoneNumber -> IO a) -> IO a
    allocaCPhoneNumber = allocaBytes {#sizeof phone_number#}

{-# INLINABLE toCPhoneNumber #-}
toCPhoneNumber :: PhoneNumber -> CPhoneNumber
toCPhoneNumber = unsafeDupablePerformIO . mask_ . c_phone_number_unmarshal

{-# INLINABLE fromCPhoneNumber #-}
fromCPhoneNumber :: CPhoneNumber -> PhoneNumber
fromCPhoneNumber = unsafeDupablePerformIO . mask_ . c_phone_number_marshal

-- These would ideally be defined in @Data.PhoneNumber.Number@ but that would
-- make them orphan instances.

-- | Compares all the data fields, consider 'Data.PhoneNumber.Util.matchNumbers'
-- instead
instance Eq PhoneNumber where
  p1 == p2 = toCPhoneNumber p1 == toCPhoneNumber p2

instance Ord PhoneNumber where
  p1 `compare` p2 = toCPhoneNumber p1 `compare` toCPhoneNumber p2
  p1 <= p2 = toCPhoneNumber p1 <= toCPhoneNumber p2

instance Show PhoneNumber where
  showsPrec d p = mangleConParen . showsPrec d (toCPhoneNumber p)
    where
      mangleConParen xs
        | Just xs' <- stripPrefix "(" xs = "(" ++ mangleCon xs'
        | otherwise = mangleCon xs
      mangleCon xs
        | Just xs' <- stripPrefix "CPhoneNumber " xs = "PhoneNumber " ++ xs'
        | otherwise = xs

instance Read PhoneNumber where
  readPrec = P.parens $ P.prec 11 $ do
    P.expectP (P.Ident "PhoneNumber")
    fromCPhoneNumber <$> P.readS_to_Prec
      (\d xs -> readsPrec d $ "CPhoneNumber" ++ xs)

-- | No internal structure
instance Data PhoneNumber where
  toConstr _ = error "Data.PhoneNumber.Number.Number.toConstr"
  gunfold _ _ = error "Data.PhoneNumber.Number.Number.gunfold"
  dataTypeOf _ = mkNoRepType "Data.PhoneNumber.Number.Number"

instance NFData PhoneNumber where
  rnf pn = pn `seq` ()
