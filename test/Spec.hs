{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either
import Data.PhoneNumber.Number
import Data.PhoneNumber.Util
import qualified Data.Set as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype CC = CC CountryCode
  deriving newtype (Show)

instance Arbitrary CC where
  arbitrary = CC . CountryCode <$> choose (-10, 1000)
  shrink (CC (CountryCode cc)) = CC . CountryCode <$> shrink cc

newtype Reg = Reg Region
  deriving newtype (Show)

instance Arbitrary Reg where
  arbitrary = do
    c1 <- elements ['A'..'Z']
    c2 <- elements ['A'..'Z']
    pure $ Reg $ Region $ BSC.pack [c1, c2]

newtype MReg = MReg (Maybe Region)
  deriving newtype (Show)

instance Arbitrary MReg where
  arbitrary = MReg . fmap (\(Reg r) -> r) <$> arbitrary
  shrink (MReg (Just _)) = [MReg Nothing]
  shrink (MReg Nothing) = []

newtype ParserInput = ParserInput BS.ByteString
  deriving newtype (Show)

instance Arbitrary ParserInput where
  arbitrary = do
    len <- choose (0, 20)
    xs <- replicateM len $ frequency
      [ (10, elements "#()+-0123456789")
      , (1, elements "'*,./:;") ]
    pure $ ParserInput $ BSC.pack xs
  shrink (ParserInput bs) = ParserInput . BS.pack <$> shrink (BS.unpack bs)

data ParsedNumber = ParsedNumber (ParseMode, Maybe Region, ParserInput) PhoneNumber
  deriving stock (Show)

instance Arbitrary ParsedNumber where
  arbitrary = do
    mode <- elements [Canonicalize, KeepRawInput]
    liftA2 ((,,) mode) arbitrary arbitrary
      `suchThatMap` \(_, MReg mRegion, ParserInput input) ->
        case parseNumber mode mRegion input of
          Right pn -> Just $ ParsedNumber (mode, mRegion, ParserInput input) pn
          _ -> Nothing
  shrink (ParsedNumber (mode, region, input) _) = do
    ParserInput input' <- shrink input
    case parseNumber mode region input' of
      Right pn -> pure $ ParsedNumber (mode, region, ParserInput input') pn
      _ -> empty

regZZ, reg001 :: Region
regZZ = "ZZ"
reg001 = "001"

cc0 :: CountryCode
cc0 = 0

main :: IO ()
main = hspec $ modifyMaxSuccess (* 10) $ do
  describe "supportedRegions" $ do
    prop "doesn't contain ZZ" $ regZZ `S.notMember` supportedRegions
    prop "doesn't contain 001" $ reg001 `S.notMember` supportedRegions

  describe "supportedGlobalNetworkCallingCodes" $ do
    prop "doesn't contain 0" $ cc0 `S.notMember` supportedGlobalNetworkCallingCodes
    prop "is a subset of supportedCallingCodes" $
      supportedGlobalNetworkCallingCodes `S.isSubsetOf` supportedCallingCodes

  describe "supportedCallingCodes" $ do
    prop "doesn't contain 0" $ cc0 `S.notMember` supportedCallingCodes

  describe "supportedTypesForRegion" $ do
    prop "doesn't contain FixedLineOrMobile" $ \(Reg region) ->
      FixedLineOrMobile `S.notMember` supportedTypesForRegion region

  describe "supportedTypesForNonGeoEntity" $ do
    prop "doesn't contain FixedLineOrMobile" $ \(CC cc) ->
      FixedLineOrMobile `S.notMember` supportedTypesForNonGeoEntity cc

  describe "convertAlphaNumber" $ do
    prop "doesn't change the parsing of a number" $ \(MReg mRegion) (ParserInput str) ->
      parseNumber Canonicalize mRegion (convertAlphaNumber str)
        === parseNumber Canonicalize mRegion str
    prop "returns a number that is not alpha" $ \(ParserInput str) ->
      not $ isAlphaNumber $ convertAlphaNumber str

  describe "normalizeNumber" $ do
    prop "returns ASCII digits unmodified" $
      forAll (elements [Digits, Dialable]) $ \mode ->
        forAll (listOf $ elements ['0'..'9']) $ \str ->
          BSC.pack str == normalizeNumber mode (BSC.pack str)
    describe "Digits" $ do
      prop "returns only ASCII digits" $ \str ->
        BSC.all (`elem` ['0'..'9']) $ normalizeNumber Digits $ BSC.pack str
      prop "doesn't randomly fail" $ \str ->
        not (BS.null $ normalizeNumber Digits $ "0" <> BS.pack str)
          .&&. not (BS.null $ normalizeNumber Digits $ BS.pack str <> "0")
    describe "Dialable" $ do
      -- PhoneNumberUtil::NormalizeDiallableCharsOnly will error on any invalid UTF8
      prop "doesn't randomly fail" $ \str ->
        not (BS.null $ normalizeNumber Digits $ "0" <> BS.pack str)
          .&&. not (BS.null $ normalizeNumber Digits $ BS.pack str <> "0")

  describe "nationalSignificantNumber" $ do
    prop "is non-empty" $ \(ParsedNumber _ pn) ->
      not $ BS.null $ nationalSignificantNumber pn
    prop "returns only ASCII digits" $ \(ParsedNumber _ pn) ->
      BSC.all (`elem` ['0'..'9']) $ nationalSignificantNumber pn
    modifyMaxDiscardRatio (* 10) $ do
      prop "concatenated with country code parses to the original (if valid)" $
        \(ParsedNumber _ pn) ->
          isValidNumber Nothing pn ==>
            let
              cc = countryCode pn
              nsn = nationalSignificantNumber pn
              str = "+" <> BSC.pack (show cc) <> nsn
            in case parseNumber Canonicalize Nothing str of
              Left err -> error $ show err
              Right pn' -> cc === countryCode pn'
                .&&. nsn === nationalSignificantNumber pn'

  describe "countryMobileToken" $ do
    prop "returns only ASCII digits" $ \(CC cc) ->
      BSC.all (`elem` ['0'..'9']) $ countryMobileToken cc

  describe "formatNumber" $ do
    prop "with International produces a parseable result (if no ext)" $
      \(ParsedNumber _ pn) -> possibleNumber Unknown pn == IsPossible ==>
        -- Apparently in the TW region an extension number is formatted
        -- with a "#", and the formatter will format
        -- "+886 2 2123 4567 ext. 1234567890" as "+886 2 2123 4567#1234567890"
        -- even though the former parses and the latter doesn't
        extension pn == Nothing ==>
          isRight $ parseNumber Canonicalize Nothing
            $ formatNumber International pn
    modifyMaxDiscardRatio (* 10) $ do
      prop "with E164 produces a parseable result (if Canonicalize)" $
        \(ParsedNumber (mode, _, _) pn) ->
          -- KeepRawInput with pn == "0000" breaks E164 formatting
          possibleNumber Unknown pn == IsPossible && mode == Canonicalize ==>
            isRight $ parseNumber Canonicalize Nothing
              $ formatNumber E164 pn
      prop "with RFC3966 produces a parseable result (if Canonicalize)" $
        \(ParsedNumber (mode, _, _) pn) ->
          -- KeepRawInput with pn == "0000" breaks RFC3966 formatting
          possibleNumber Unknown pn == IsPossible && mode == Canonicalize ==>
            isRight $ parseNumber Canonicalize Nothing
              $ formatNumber RFC3966 pn

  describe "truncateTooLongNumber" $ do
    pure ()

  describe "numberType" $ do
    pure ()

  describe "isValidNumber" $ do
    modifyMaxDiscardRatio (* 10) $ do
      prop "is preserved under parse . format" $ \(ParsedNumber _ pn) ->
        isValidNumber Nothing pn ==>
          case parseNumber Canonicalize Nothing $ formatNumber E164 pn of
            Left err -> error $ show err
            Right pn' -> isValidNumber Nothing pn'

  describe "regionForNumber" $ do
    prop "doesn't return ZZ" $ \(ParsedNumber _ pn) ->
      regionForNumber pn =/= Just (Right regZZ)
    prop "doesn't return 001" $ \(ParsedNumber _ pn) ->
      regionForNumber pn =/= Just (Right reg001)
    prop "returns a supported region" $ \(ParsedNumber _ pn) ->
      case regionForNumber pn of
        Just (Right region) -> region `S.member` supportedRegions
        _ -> discard
    prop "returns Region001 exactly for supported non-geo entities" $
      \(ParsedNumber _ pn) ->
        (countryCode pn `S.member` supportedGlobalNetworkCallingCodes)
          === (regionForNumber pn == Just (Left Region001))

  describe "countryCodeForRegion" $ do
    prop "doesn't return 0" $ \(Reg region) ->
      countryCodeForRegion region =/= Just cc0
    prop "returns codes in supportedCallingCodes" $ \(Reg region) ->
      case countryCodeForRegion region of
        Nothing -> discard
        Just cc -> cc `S.member` supportedCallingCodes

  describe "regionForCountryCode" $ do
    prop "doesn't return ZZ" $ \(CC cc) ->
      regionForCountryCode cc =/= Just (Right regZZ)
    prop "doesn't return 001" $ \(CC cc) ->
      regionForCountryCode cc =/= Just (Right reg001)
    prop "returns Region001 exactly for supported non-geo entities" $ \(CC cc) ->
      (cc `S.member` supportedGlobalNetworkCallingCodes)
        === (regionForCountryCode cc == Just (Left Region001))

  describe "regionsForCountryCode" $ do
    prop "doesn't return ZZ" $ \(CC cc) ->
      Right regZZ `notElem` regionsForCountryCode cc
    prop "doesn't return 001" $ \(CC cc) ->
      Right reg001 `notElem` regionsForCountryCode cc

  describe "isNANPACountry" $ do
    pure ()

  describe "nddPrefixForRegion" $ do
    prop "with True returns only ASCII digits" $ \(Reg region) ->
      BSC.all (`elem` ['0'..'9']) $ nddPrefixForRegion True region

  describe "possibleNumber" $ do
    modifyMaxDiscardRatio (* 10) $ do
      prop "with Unknown is more lenient than isValidNumber" $
        \(ParsedNumber _ pn) -> isValidNumber Nothing pn ==>
          possibleNumber Unknown pn `elem` [IsPossible, IsPossibleLocalOnly]

  describe "canBeInternationallyDialed" $ do
    pure ()

  describe "isGeographicalNumber" $ do
    prop "matches isGeographicalNumberType" $ \(ParsedNumber _ pn) ->
      isGeographicalNumber pn
        === isGeographicalNumberType (numberType pn) (countryCode pn)

  describe "isGeographicalNumberType" $ do
    pure ()

  describe "parseNumber" $ do
    prop "doesn't return a number with CountryCode 0" $ \(ParsedNumber _ pn) ->
      countryCode pn /= cc0
