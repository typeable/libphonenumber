{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -with-rtsopts=-V0 -with-rtsopts=-C0 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Exception
import Control.Monad
import Data.PhoneNumber.Number
import Data.PhoneNumber.Util
import System.Environment
import System.Exit
import System.Mem
import System.Process
import System.Timeout

-- If not currently running under valrind, start ourselves under valgrind and
-- exit immediately after that process exits.
restartWithValgrind :: IO ()
restartWithValgrind = lookupEnv envVar >>= \case
  Nothing -> do
    setEnv envVar "1"
    prog <- getExecutablePath
    args <- getArgs
    (_, _, _, hdl) <- createProcess (proc "valgrind" $ valgrindOpts ++ prog:args)
      { delegate_ctlc = True
      }
    exitWith =<< waitForProcess hdl
  Just _ -> pure ()
  where
    envVar = "RUNNING_IN_VALGRIND"
    valgrindOpts =
      [ "--leak-check=full"
      , "--show-leak-kinds=all"
      , "--errors-for-leak-kinds=all"
      , "--num-callers=30"
      , "--suppressions=test/libphonenumber.supp"
      , "--error-exitcode=1"
      ]

-- Repeatedly re-run the given computation a given number of times, attempting
-- to interrupt it with an async exception with various delays. The delays are
-- roughly uniformly distributed over a range at the end of which the
-- computation stops timing out.
--
-- If there was a place where an unmasked async exception could fire while we
-- have pointers to allocated data that have not been accounted for by the
-- haskell side, this has a good chance to catch it.
{-# NOINLINE hammerIO #-}
hammerIO :: Int -> IO a -> IO ()
hammerIO total act = go total 1 1
  where
    go 0 !_ !_ = pure ()
    go tot limit cur = timeout cur (void act) >>= \case
      Nothing
        | cur == limit -> go (tot - 1) (limit + 1) (cur + 1)
        | otherwise    -> go (tot - 1) limit (cur + 1)
      Just ()
        | cur == limit -> go (tot - 1) limit 1
        | otherwise    -> go (tot - 1) limit (cur + 1)

{-# NOINLINE hammer #-}
hammer :: Int -> (a -> b) -> a -> IO ()
hammer total f x = hammerIO total $ evaluate . f =<< evaluate x

main :: IO ()
main = do
  restartWithValgrind
  let
    iter = 100000 -- starts catching stuff at around 1000
    region = "US"
    countryCallingCode = 1
    carrierCode = "10"
    nonGeoCode = 800
    phoneString = "+41 44 668 1800"
    Right phoneNumber = parseNumber KeepRawInput Nothing phoneString
  hammer iter (\(PhoneNumber a b c d e f g h) -> PhoneNumber a b c d e f g h) phoneNumber
  hammer iter supportedTypesForRegion region
  hammer iter supportedTypesForNonGeoEntity nonGeoCode
  hammer iter isAlphaNumber phoneString
  hammer iter convertAlphaNumber phoneString
  hammer iter (normalizeNumber Digits) phoneString
  hammer iter (normalizeNumber Dialable) phoneString
  hammer iter nationalSignificantNumber phoneNumber
  hammer iter countryMobileToken countryCallingCode
  hammer iter (formatNumber E164) phoneNumber
  hammer iter (formatNumber International) phoneNumber
  hammer iter (formatNumber National) phoneNumber
  hammer iter (formatNumber RFC3966) phoneNumber
  hammer iter (formatNumber (NationalWithCarrierCodeOverride carrierCode)) phoneNumber
  hammer iter (formatNumber (NationalWithCarrierCodeFallback carrierCode)) phoneNumber
  hammer iter (formatNumber (ForMobileDialing region True)) phoneNumber
  hammer iter (formatNumber (Original region)) phoneNumber
  hammer iter (formatNumber (OutOfCountry region False)) phoneNumber
  hammer iter (formatNumber (OutOfCountry region False)) phoneNumber
  hammer iter truncateTooLongNumber phoneNumber
  hammer iter numberType phoneNumber
  hammer iter (isValidNumber (Just (Right region))) phoneNumber
  hammer iter regionForNumber phoneNumber
  hammer iter countryCodeForRegion region
  hammer iter regionForCountryCode countryCallingCode
  hammer iter regionsForCountryCode countryCallingCode
  hammer iter isNANPACountry region
  hammer iter (nddPrefixForRegion False) region
  hammer iter (possibleNumber Unknown) phoneNumber
  hammer iter canBeInternationallyDialed phoneNumber
  hammer iter isGeographicalNumber phoneNumber
  hammer iter (isGeographicalNumberType FixedLine) countryCallingCode
  hammer iter (parseNumber Canonicalize (Just region)) phoneString
  hammer iter (parseNumber Canonicalize (Just region)) ""
  hammer iter (parseNumber KeepRawInput (Just region)) phoneString
  hammer iter (parseNumber KeepRawInput (Just region)) ""
  hammer iter (matchNumbers (Left phoneString)) (Left phoneString)
  hammer iter (matchNumbers (Left phoneString)) (Right phoneNumber)
  hammer iter (matchNumbers (Right phoneNumber)) (Left phoneString)
  hammer iter (matchNumbers (Right phoneNumber)) (Right phoneNumber)
  performGC
