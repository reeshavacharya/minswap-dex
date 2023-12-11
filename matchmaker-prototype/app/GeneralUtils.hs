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
  -- Debug.traceM (BS8.unpack scriptCborBS)
  Debug.traceM ("\n\n\n")
  unhexed <- case unHexLazy (T.decodeUtf8 scriptCborBS) of
    Nothing -> fail "UNEXPECTED: failed to decode cbor."
    Just bs -> pure bs
  let encodedCbor = toStrictByteString $ encodeBytes $ scriptCborBS
  -- Debug.traceM (toHexString encodedCbor)
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

batcher = parseToAddressInEra "addr1qydzppfxwjnp8zqq5py65azlh07ydf8f72j2p6ptfzvh6uhee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdsjcs99a"

batcherUtxo =
  ( TxIn (CApi.TxId "58cbd85ad2e69aefb0f48c66932932ab609b36068e5ad26991a0a86c8bfe5647") (TxIx 2),
    CApi.TxOut batcher (TxOutValue MultiAssetInBabbageEra batcherValue) TxOutDatumNone ReferenceScriptNone --503000000 lovelace
  )
  where
    licenseSymbol = parseStringToAssetId "2f2e0404310c106e2a260e8eb5a7e43f00cff42c667489d30e179816.2702028267"

    batcherValue = valueFromList [(AdaAssetId, 503000000), (licenseSymbol, 1)]

genSomeWalelt :: NetworkId -> Gen TxBuilder
genSomeWalelt netid = do
  vkeys <- Gen.list (Range.linear 2 5) (genVerificationKey AsPaymentKey)
  userStake <- genStakeAddressReference
  let addresses = [parseToAddressInEra "addr1qydzppfxwjnp8zqq5py65azlh07ydf8f72j2p6ptfzvh6uhee858y3kj7qmn3pvfdtfgqjmj99nnypx2eysgx3wpafdsjcs99a"]
      genutxo :: Gen Value -> Gen (TxIn, TxOut ctx BabbageEra)
      genutxo genVal = do
        value <- genVal
        txid <- genTxIn
        address <- element addresses
        pure (txid, TxOut address (TxOutValue MultiAssetInBabbageEra value) TxOutDatumNone ReferenceScriptNone)
      genAdaVal :: Gen Value
      genAdaVal = do
        amount <- Gen.integral (Range.linear 2_000_000 3_000_000_000_000) <&> Quantity
        pure $ valueFromList [(AdaAssetId, amount)]
      genCollateralVal :: Gen Value
      genCollateralVal = do
        amount <- Gen.integral (Range.linear 5_000_000 10_000_000) <&> Quantity
        pure $ valueFromList [(AdaAssetId, amount)]
  utxos <- Gen.list (Range.linear 4 10) (genutxo genValueForTxOut)
  adaUtxos <- Gen.list (Range.linear 4 10) (genutxo genAdaVal)
  collateralUtxo <- genutxo genCollateralVal
  pure $ txWalletUtxos (UTxO . Map.fromList $ collateralUtxo : utxos <> adaUtxos <> [batcherUtxo])

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