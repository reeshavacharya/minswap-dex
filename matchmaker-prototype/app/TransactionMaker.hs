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
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Either (fromRight)
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe
import Data.Maybe (fromJust)
import Data.Text
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Debug.Trace as Debug
import GeneralUtils
import Hedgehog
import Hedgehog.Gen
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf, flattenValue)
import PlutusTx.Builtins (emptyByteString, sha2_256)
import PlutusTx.Prelude (consByteString, divide, quotient, remainder)
import Types

minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48

integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString

mkNFTTokenName :: TxOutRef -> TokenName
mkNFTTokenName (TxOutRef refHash refIdx) = tokenName
  where
    tokenName :: TokenName
    tokenName = TokenName $ sha2_256 $ Plutus.V1.Ledger.Api.getTxId refHash <> integerToBS refIdx

poolContractAddress = parseToAddressInEra "addr1z8snz7c4974vzdpxu65ruphl3zjdvtxw8strf2c2tmqnxz2j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq0xmsha"

lpTokenCurrencySymbol = "e4214b7cce62ac6fbba385d164df48e157eae5863521b4b67ca71d86"

factoryTokenAssetId = parseStringToAssetId "13aa2accf2e1561723aa26871e071fdf32c867cff7e7d50ad470d62f.MINSWAP"

poolNFTCurrencySymbol = "0be55d262b29f564998ff81efe21bdc0022621c12f15af08d0f2ddb1"

orderContractAddress = parseToAddressInEra "addr1zxn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uw6j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq6s3z70"

poolScript :: (MonadBlockfrost m, MonadFail m) => m (CApi.Script PlutusScriptV1)
poolScript = eitherScript (addrInEraToPlutusAddress poolContractAddress)

orderScript :: (MonadBlockfrost m, MonadFail m) => m (CApi.Script PlutusScriptV1)
orderScript = eitherScript (addrInEraToPlutusAddress orderContractAddress)

getAmountOut :: Integer -> Integer -> Integer -> Integer
getAmountOut reserveA reserveB inA =
  let inAWithFee = inA * 997
      numerator = inAWithFee * reserveB
      denominator = reserveA * 1000 + inAWithFee
      outB = numerator `divide` denominator
   in outB

makeSeiTx :: OrderMatcher -> CApi.Script PlutusScriptV1 -> PoolMatches -> CApi.Script PlutusScriptV1 -> GenT Identity TxBuilder
makeSeiTx order orderScript pool poolScript = do
  wallet <- genSomeWalelt Mainnet
  let receiver = plutusAddrToBabbage Mainnet (odReceiver $ omOrderDatum order)
      desiredAssetClass = plutusAssetClassToAssetId $ seiDesiredCoin $ odStep $ omOrderDatum order
      amountIn =
        assetClassValueOf (omOfferedAmount order) (fst $ pmmOffered pool)
          - (odBatcherFee (omOrderDatum order) + odOutputADA (omOrderDatum order))
      amountOut = getAmountOut (snd $ pmmOffered pool) (snd $ pmmDesired pool) amountIn
  let payReceiver = valueFromList [(desiredAssetClass, Quantity amountOut), (AdaAssetId, Quantity (odOutputADA $ omOrderDatum order))]
      coinAOutput = pdCoinA $ pmmDatum pool
      poolValue = pmmValue pool
      lpTokenInPool = lpTokenCurrencySymbol `valueIn` poolValue
      factoryTokenInPool = valueFromList [(factoryTokenAssetId, 1)]
      poolNftInPool = poolNFTCurrencySymbol `valueIn` poolValue
      payPool = calculatePoolOutput pool amountOut amountIn <> lpTokenInPool <> factoryTokenInPool <> poolNftInPool
      poolDatum = dataToScriptData $ pmmDatum pool
      batcherPkh = case addrInEraToPkh batcher of
        Just a -> a
        _ -> error "Batcher address does not have a pubKey."
      poolRedeemer = dataToScriptData $ ApplyPool (addrInEraToPlutusAddress batcher) 2
      orderRedeemer = dataToScriptData ApplyOrder
      poolNFTUtxo =
        ( parseTxIn (pmmDesiredUtxo pool),
          toCtxUTxOTxOut $ CApi.TxOut poolContractAddress (TxOutValue MultiAssetInBabbageEra (fromPlutusValue poolValue)) (TxOutDatumInTx ScriptDataInBabbageEra (dataToScriptData $ pmmDatum pool)) ReferenceScriptNone
        )
      orderUtxo =
        ( parseTxIn (omOrderUtxo order),
          toCtxUTxOTxOut $ CApi.TxOut orderContractAddress (TxOutValue MultiAssetInBabbageEra (fromPlutusValue (omOfferedAmount order))) (TxOutDatumInTx ScriptDataInBabbageEra (dataToScriptData $ omOrderDatum order)) ReferenceScriptNone
        )
      txb =
        txPayTo receiver payReceiver
          <> txPayTo batcher (valueFromList [(AdaAssetId, Quantity (odBatcherFee $ omOrderDatum order))])
          <> txPayToScriptWithDataInTx poolContractAddress payPool poolDatum
          <> uncurry txRedeemUtxoWithDatum poolNFTUtxo poolScript poolDatum poolRedeemer Nothing
          <> uncurry txRedeemUtxoWithDatum orderUtxo orderScript (dataToScriptData $ omOrderDatum order) orderRedeemer Nothing
          <> uncurry txConsumeUtxo batcherUtxo
          <> txSignByPkh batcherPkh
          <> wallet
  pure txb

calculatePoolOutput pool amountOut amountIn =
  let offeredCoin = plutusAssetClassToAssetId $ fst $ pmmOffered pool
      desiredCoin = plutusAssetClassToAssetId $ fst $ pmmDesired pool
      desiredCoinAmountInPool = snd $ pmmDesired pool
      offeredCoinAmountInPool = snd $ pmmOffered pool
      val = valueFromList [(desiredCoin, Quantity desiredCoinAmountInPool - Quantity amountOut), (offeredCoin, Quantity offeredCoinAmountInPool + Quantity amountIn)]
   in val
