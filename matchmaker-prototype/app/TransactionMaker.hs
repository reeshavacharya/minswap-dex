{-# LANGUAGE OverloadedStrings #-}

module TransactionMaker where

import Blockfrost.Client (MonadBlockfrost, ScriptCBOR, getScriptCBOR)
import Blockfrost.Types (ScriptCBOR (ScriptCBOR), ScriptHash (..))
import Cardano.Api
import qualified Cardano.Api as CApi
import Cardano.Api.Shelley (ReferenceScript (..), ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra), toPlutusData)
import Cardano.Binary
import Cardano.Kuber.Api
import Cardano.Kuber.Util
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Either (fromRight)
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Debug.Trace as Debug
import GeneralUtils
import Hedgehog
import Hedgehog.Gen
import OrderUtils
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval (after)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValueOf, flattenValue)
import PlutusTx.Builtins (emptyByteString, sha2_256)
import PlutusTx.Prelude (consByteString, divide, quotient, remainder)
import Types

batcher = parseToAddressInEra "addr1qx7tzh4qen0p50ntefz8yujwgqt7zulef6t6vrf7dq4xa82j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pqrkj6fh"

poolContractAddress = parseToAddressInEra "addr1z8snz7c4974vzdpxu65ruphl3zjdvtxw8strf2c2tmqnxz2j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq0xmsha"

lpTokenCurrencySymbol = "e4214b7cce62ac6fbba385d164df48e157eae5863521b4b67ca71d86"

factoryTokenAssetId = parseStringToAssetId "13aa2accf2e1561723aa26871e071fdf32c867cff7e7d50ad470d62f.MINSWAP"

poolNFTCurrencySymbol = "0be55d262b29f564998ff81efe21bdc0022621c12f15af08d0f2ddb1"

orderContractAddress = parseToAddressInEra "addr1zxn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uw6j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq6s3z70"

poolScript :: (MonadBlockfrost m, MonadFail m) => m (CApi.Script PlutusScriptV1)
poolScript = eitherScript (addrInEraToPlutusAddress poolContractAddress)

orderScript :: (MonadBlockfrost m, MonadFail m) => m (CApi.Script PlutusScriptV1)
orderScript = eitherScript (addrInEraToPlutusAddress orderContractAddress)

batcherTokenPolicyId = case parseStringToAssetId
  "2f2e0404310c106e2a260e8eb5a7e43f00cff42c667489d30e179816.POSIXTime" of
  AdaAssetId -> error "IMPOSSIBLE"
  AssetId pi an -> pi

getBatcherUtxo time = do
  (UTxO utxoMap) <- kQueryUtxoByAddress (Set.singleton $ addressInEraToAddressAny batcher)
  let utxoLists = Map.toList utxoMap
      utxoWithToken =
        Data.Maybe.mapMaybe
          ( \(ti, to) -> case to of
              CApi.TxOut aie tov tod rs -> case tov of
                TxOutAdaOnly oasie lo -> Nothing
                TxOutValue masie va ->
                  Just $
                    Data.Maybe.mapMaybe
                      ( \(ai, quan) -> case ai of
                          AdaAssetId -> Nothing
                          AssetId pi an ->
                            if (pi == batcherTokenPolicyId)
                              && (case ti of TxIn _ (TxIx wrd) -> toInteger wrd <= 2)
                              && (bbsToInteger (toBuiltin (serialiseToRawBytesHex an))) > time
                              then Just (UTxO $ Map.fromList [(ti, to)])
                              else Nothing
                      )
                      (valueToList va)
          )
          utxoLists
  if Prelude.null utxoWithToken
    then error "Batcher is broke"
    else
      if Prelude.null (Prelude.head utxoWithToken)
        then error "Batcher has no tokens."
        else pure $ Prelude.head (Prelude.head utxoWithToken)

getbatcherUtxo' :: (HasChainQueryAPI api, HasKuberAPI api) => Kontract api w FrameworkError (UTxO BabbageEra)
getbatcherUtxo' = do
  posix <- liftIO time
  getBatcherUtxo 1602627200000
  -- getBatcherUtxo posix
  where
    time = do
      getTime

makeSeiTx order orderScript pool poolScript time batcherUtxo = do
  let -- Setting up variables for calculations
      desiredAssetClass = plutusAssetClassToAssetId $ seiDesiredCoin $ odStep $ omOrderDatum order

      (coinIn, reserveIn, reserveOut) =
        if pdCoinA (pmmDatum pool) /= fst (pmmDesired pool)
          then (pdCoinA (pmmDatum pool), snd (pmmOffered pool), snd (pmmDesired pool))
          else (pdCoinB (pmmDatum pool), snd (pmmOffered pool), snd (pmmDesired pool))

      coinInAmount = assetClassValueOf (omOfferedAmount order) coinIn

      amountIn =
        if coinIn == assetClass adaSymbol adaToken
          then
            coinInAmount
              - (odBatcherFee (omOrderDatum order) + odOutputADA (omOrderDatum order))
          else coinInAmount

      amountOut = getAmountOut reserveIn reserveOut amountIn

      -- Pay Receiver Calculations
      receiver = plutusAddrToBabbage Mainnet (odReceiver $ omOrderDatum order)

      (desiredCoinAmountOut, mAdaAmountOut)
        | fst (pmmDesired pool) == assetClass adaSymbol adaToken = (amountOut + odOutputADA (omOrderDatum order), Nothing)
        | otherwise = (amountOut, Just (odOutputADA (omOrderDatum order)))

      payReceiverDesiredCoin = valueFromList [(desiredAssetClass, Quantity desiredCoinAmountOut)]

      payReceiverAda = case mAdaAmountOut of
        Nothing -> mempty
        Just n -> valueFromList [(AdaAssetId, Quantity n)]

      -- Pay Batcher Calculations
      batcherPkh = case addrInEraToPkh batcher of
        Just a -> a
        _ -> error "Batcher address does not have a pubKey."

      payBatcher = case batcherUtxo of
        (ti, to) -> case to of
          CApi.TxOut aie tov tod rs -> case tov of
            TxOutAdaOnly oasie lo -> error "bater should have token"
            TxOutValue masie va -> va <> valueFromList [(AdaAssetId, Quantity (odBatcherFee $omOrderDatum order))]

      batcherIndex = case batcherUtxo of (ti, to) -> case ti of TxIn ti' ti3 -> case ti3 of TxIx wo -> toInteger wo
      -- pay Pool Calculations

      poolValue = pmmValue pool

      lpTokenInPool = lpTokenCurrencySymbol `valueIn` poolValue

      factoryTokenInPool = valueFromList [(factoryTokenAssetId, 1)]

      poolNftInPool = poolNFTCurrencySymbol `valueIn` poolValue

      payPool = calculatePoolOutput pool amountOut amountIn <> lpTokenInPool <> factoryTokenInPool <> poolNftInPool

      poolDatum = dataToScriptData $ pmmDatum pool

      poolRedeemer = dataToScriptData $ ApplyPool (addrInEraToPlutusAddress batcher) batcherIndex

      orderRedeemer = dataToScriptData ApplyOrder

      poolUtxo =
        ( parseTxIn (pmmDesiredUtxo pool),
          toCtxUTxOTxOut $ CApi.TxOut poolContractAddress (TxOutValue MultiAssetInBabbageEra (fromPlutusValue poolValue)) (TxOutDatumInTx ScriptDataInBabbageEra (dataToScriptData $ pmmDatum pool)) ReferenceScriptNone
        )

      orderUtxo =
        ( parseTxIn (omOrderUtxo order),
          toCtxUTxOTxOut $ CApi.TxOut orderContractAddress (TxOutValue MultiAssetInBabbageEra (fromPlutusValue (omOfferedAmount order))) (TxOutDatumInTx ScriptDataInBabbageEra (dataToScriptData $ omOrderDatum order)) ReferenceScriptNone
        )

      txb =
        txPayTo receiver (payReceiverDesiredCoin <> payReceiverAda)
          <> txPayTo batcher payBatcher
          <> txPayToScriptWithDataInTx poolContractAddress payPool poolDatum
          <> uncurry txRedeemUtxoWithDatum poolUtxo poolScript poolDatum poolRedeemer Nothing
          <> uncurry txRedeemUtxoWithDatum orderUtxo orderScript (dataToScriptData $ omOrderDatum order) orderRedeemer Nothing
          <> txSignByPkh batcherPkh
          <> txValidUntilPosixTime time
          <> txWalletAddress batcher
  pure txb

calculatePoolOutput pool amountOut amountIn =
  let coinA = plutusAssetClassToAssetId $ pdCoinA (pmmDatum pool)
      coinB = plutusAssetClassToAssetId $ pdCoinB (pmmDatum pool)
      desiredCoinAmountInPool = snd $ pmmDesired pool
      offeredCoinAmountInPool = snd $ pmmOffered pool
      coinA_afterSwap = if pdCoinA (pmmDatum pool) /= fst (pmmDesired pool) then offeredCoinAmountInPool + amountIn else offeredCoinAmountInPool - amountOut
      coinB_AfterSwap = if pdCoinA (pmmDatum pool) /= fst (pmmDesired pool) then desiredCoinAmountInPool - amountOut else desiredCoinAmountInPool + amountIn
      val = valueFromList [(coinA, Quantity coinA_afterSwap), (coinB, Quantity coinB_AfterSwap)]
   in val
