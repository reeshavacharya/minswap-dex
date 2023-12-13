{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GeneralUtils where

import Blockfrost.Client (MonadBlockfrost, ScriptCBOR (ScriptCBOR), ScriptHash (..), getScriptCBOR)
import Cardano.Api
import qualified Cardano.Api as CApi
import Cardano.Api.Shelley
import Cardano.Binary
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Short as SBS
import Data.Functor
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import GHC.Base (map)
import qualified GHC.Word as Word
import Gen.Cardano.Api.Typed
import Hedgehog
import Hedgehog.Gen hiding (map)
import qualified Hedgehog.Gen as Gen hiding (map)
import qualified Hedgehog.Range as Range
import qualified Plutus.V1.Ledger.Address as PlutusAddress
import Plutus.V1.Ledger.Api (adaSymbol, adaToken, fromBuiltin)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), CurrencySymbol (CurrencySymbol), TokenName (TokenName), assetClass, assetClassValue, flattenValue)
import Plutus.V2.Ledger.Api (Credential (..), ValidatorHash (..))
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified Plutus.V2.Ledger.Tx as Plutus
import PlutusTx.Prelude (divide)
import PlutusTx.Builtins (BuiltinByteString, lengthOfByteString, indexByteString)
import Data.Time (getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX

parseToPlutusTxId :: Plutus.TxId -> [Char]
parseToPlutusTxId txid = case txid of Plutus.TxId bbs -> BS8.unpack $ fromBuiltin bbs

toAddrInBabbage :: Cardano.Api.Address ShelleyAddr -> AddressInEra BabbageEra
toAddrInBabbage shelly = case anyAddressInEra BabbageEra (toAddressAny shelly) of
  Just aie -> aie

parseToAddressInEra :: String -> AddressInEra BabbageEra
parseToAddressInEra str = case parseAddress (T.pack str) of
  Just aa -> aa
  _ -> Prelude.error "could not parse to address."

parseStringToAssetId :: String -> AssetId
parseStringToAssetId str = case parseAssetId (T.pack str) of
  Just a -> a
  _ -> Prelude.error "could not parse to assetId"

plutusAssetClassToAssetId :: AssetClass -> AssetId
plutusAssetClassToAssetId plutusAssetClass =
  if plutusAssetClass == assetClass adaSymbol adaToken
    then AdaAssetId
    else case plutusAssetClass of
      AssetClass (CurrencySymbol bbs, TokenName bbs') ->
        AssetId
          (fromJust $ deserialiseFromRawBytes AsPolicyId (fromBuiltin bbs))
          (fromJust $ deserialiseFromRawBytes AsAssetName (fromBuiltin bbs'))

parseTuple :: String -> (String, String)
parseTuple input =
  let [firstPart, secondPart] = T.splitOn "," (T.pack (init $ tail input))
   in (T.unpack firstPart, init $ tail (T.unpack secondPart))

parseToPlutusAddress str = addrInEraToPlutusAddress $ parseToAddressInEra str

plutusAddrToBabbage netId plutusAddr = toAddrInBabbage $ fromJust $ fromPlutusAddress netId plutusAddr

toPlutusValue val1 = mconcat $ Prelude.map (\(asset, Quantity amount) -> assetClassValue (toPlutusAssetClass asset) amount) (valueToList val1)

plutusAddrToText addr = serialiseAddress $ toAddrInBabbage $ fromJust $ fromPlutusAddress Mainnet addr

parseTxIn (id, ix) = TxIn (parseTxId id) (TxIx $ fromInteger ix)

getScriptHash plutusAddress = case plutusAddress of
  PlutusAddress.Address cre m_sc -> case cre of
    PubKeyCredential pkh -> error "Address is not a script address."
    ScriptCredential vh -> case vh of ValidatorHash bbs -> bbs

cborTextFromAddress plutusAddress = case deserialiseFromRawBytes
  AsPolicyId
  (fromBuiltin $ getScriptHash plutusAddress) of
  Nothing -> error "Error while getting text representation of scriptHash."
  Just pi -> case pi of PolicyId sh -> T.pack $ init (tail $ show sh)

scriptCBOR plutusAddress = getScriptCBOR (ScriptHash $ cborTextFromAddress plutusAddress)

eitherScript :: (MonadBlockfrost m, MonadFail m) => Plutus.Address -> m (CApi.Script PlutusScriptV1)
eitherScript plutusAddress = do
  scriptCborBS <- scriptBS plutusAddress
  unhexed <- case unHexLazy (T.decodeUtf8 scriptCborBS) of
    Nothing -> fail "UNEXPECTED: failed to decode cbor."
    Just bs -> pure bs
  let encodedCbor = toStrictByteString $ encodeBytes $ scriptCborBS
  pure $ PlutusScript PlutusScriptV1 $ PlutusScriptSerialised $ SBS.toShort (toStrict unhexed)

scriptBS plutusAddress = do
  res <- scriptCBOR plutusAddress
  case res of
    ScriptCBOR m_txt -> case m_txt of
      Nothing -> error "invalid script cbor"
      Just txt -> pure $ BS8.pack $ T.unpack txt

parseTxId :: String -> TxId
parseTxId id = case deserialiseFromRawBytesHex AsTxId (BS8.pack id) of
  Left rbhe -> error "unable to parse to txid"
  Right ti -> ti

fromPlutusValue :: Plutus.Value -> Value
fromPlutusValue val = mconcat $ map (\(cs, tn, n) -> valueFromList [(plutusAssetClassToAssetId (assetClass cs tn), Quantity n)]) (flattenValue val)

valueIn currencySymbol value =
  mconcat $
    Data.Maybe.mapMaybe
      ( \(cs, tn, n) ->
          if cs == currencySymbol
            then Just $ valueFromList [(plutusAssetClassToAssetId (assetClass cs tn), Quantity n)]
            else Nothing
      )
      (flattenValue value)

-- 916730 47940671942 1346
getAmountOut :: Integer -> Integer -> Integer -> Integer
getAmountOut reserveA reserveB inA =
  let inAWithFee = inA * 997
      numerator = inAWithFee * reserveB
      denominator = reserveA * 1000 + inAWithFee
      outB = numerator `divide` denominator
   in outB
-- 1346 916730 47940671942
calculateSwapExactIn :: Integer -> Integer -> Integer -> (Integer, Integer)
calculateSwapExactIn amountIn reserveIn reserveOut =
  let amountIn' = amountIn
      reserveIn' = reserveIn
      reserveOut' = reserveOut

      amtOutNumerator = amountIn' * 997 * reserveOut'
      amtOutDenominator = amountIn' * 997 + reserveIn' * 1000

      priceImpactNumerator =
        reserveOut' * amountIn' * amtOutDenominator * 997
          - amtOutNumerator * reserveIn' * 1000
      priceImpactDenominator =
        reserveOut' * amountIn' * amtOutDenominator * 1000

      amountOut = amtOutNumerator `divide` amtOutDenominator
      priceImpact =
        let num = priceImpactNumerator * 100
            denom = priceImpactDenominator
         in num `divide` denom
   in (amountOut, priceImpact)

genStakeAddressReference :: GenT Identity StakeAddressReference
genStakeAddressReference =
  choice [pure NoStakeAddress, genStakeCredential]

genStakeCredential :: GenT Identity StakeAddressReference
genStakeCredential = do
  vKeyStake <- genVerificationKey AsStakeKey
  pure (StakeAddressByValue $ StakeCredentialByKey $ verificationKeyHash vKeyStake)

genPubkeyAddressShelley :: NetworkId -> Gen (Address ShelleyAddr)
genPubkeyAddressShelley netid = do
  vKeyPayment <- genVerificationKey AsPaymentKey
  makeShelleyAddress
    netid
    (PaymentCredentialByKey $ verificationKeyHash vKeyPayment)
    <$> genStakeCredential

bbsToInteger :: BuiltinByteString -> Integer
bbsToInteger input = go 0 0
  where
    len = lengthOfByteString input

    go :: Integer -> Integer -> Integer
    go idx acc
      | idx == len = acc
      | idx == 0 && byte == 45 = negate $ go (idx + 1) acc
      | byte < 48 || byte > 48 + 9 = error "bbsToInteger failed"
      | otherwise = go (idx + 1) (acc * 10 + (byte - 48))
      where
        byte = indexByteString input idx


getTime = do
  currentTime <- getCurrentTime
  let oneHourLater = addUTCTime 3600 currentTime
      posixTime = utcTimeToPOSIXSeconds oneHourLater
  pure posixTime