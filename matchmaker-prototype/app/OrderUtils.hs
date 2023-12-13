{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OrderUtils where

import Blockfrost.Client
import Blockfrost.Client.Cardano.Accounts
import Blockfrost.Types.Cardano.Scripts
import Cardano.Api
import Cardano.Api.Shelley (scriptDataFromJsonDetailedSchema, toPlutusData)
import Cardano.Crypto.Hash (ByteString)
import Cardano.Kuber.Api
import Cardano.Kuber.Data.Models (Wrapper (unWrap))
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
import Control.Exception
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set.Internal as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Debug.Trace as Debug
import GHC.IO
import GeneralUtils (parseToPlutusTxId)
import Money (Discrete, Discrete', someDiscreteAmount, someDiscreteCurrency)
import Network.HTTP.Client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Simple (setRequestHeaders)
import Plutus.V1.Ledger.Api (TxOutRef (TxOutRef))
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol (CurrencySymbol), TokenName (TokenName), Value, adaSymbol, adaToken, assetClass, assetClassValue)
import PlutusTx.Prelude (BuiltinData, toBuiltin)
import System.Directory
import Types (OrderDatum (odStep), OrderMatcher (OrderMatcher), OrderStep (..), unDatumResponse)

orderAddressInfo :: MonadBlockfrost m => m [AddressUtxo]
orderAddressInfo = do
  getAddressUtxos (Address (T.pack "addr1zxn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uw6j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq6s3z70"))

-- https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/{datum_hash}
generateOrderMatcher :: (MonadBlockfrost m) => m [OrderMatcher]
generateOrderMatcher = do
  addressUtxos <- orderAddressInfo
  let datums =
        foldr
          ( \x acc -> case x of
              AddressUtxo ad th n ams bh m_dh m_id m_sh -> case m_dh of
                Nothing -> acc
                Just dh -> case dh of
                  DatumHash txt -> (mconcat $ amountToValue ams, datumHashToInlineDatumSync (T.unpack txt), txHashtoTxOut th n) : acc
          )
          []
          addressUtxos
  let result =
        foldr
          ( \(va, bs, th) acc -> case getOrderDatum bs of
              Nothing -> acc
              Just od -> OrderMatcher od va (case th of TxOutRef ti n -> (parseToPlutusTxId ti, n)) : acc
          )
          []
          datums
  return result

txHashtoTxOut :: TxHash -> Integer -> Plutus.TxOutRef
txHashtoTxOut txHash index = case txHash of
  TxHash txt -> Plutus.TxOutRef (Plutus.TxId $ toBuiltin $ BS8.pack $ T.unpack txt) index

-- TxOutRef (Plutus.TxId )

amountToValue :: [Amount] -> [Plutus.V1.Ledger.Value.Value]
amountToValue =
  foldr
    ( \x acc -> case x of
        AdaAmount dis ->
          assetClassValue (assetClass adaSymbol adaToken) (toInteger dis) :
          acc
        AssetAmount sd ->
          assetClassValue (uncurry assetClass (getAssetClass sd)) (toInteger $ someDiscreteAmount sd) :
          acc
    )
    []
  where
    getAssetClass sd = splitText $ someDiscreteCurrency sd

splitText :: T.Text -> (CurrencySymbol, TokenName)
splitText t =
  let (first56, rest) = T.splitAt 56 t
      toCurrencySymbol bs = case unHex bs of
        Just (binary :: BS.ByteString) -> CurrencySymbol (toBuiltin binary)
        Nothing -> Prelude.error "Error parsing CurrencySymbol"
      toTokenName bs = case unHex bs of
        Just (binary :: BS.ByteString) -> TokenName (toBuiltin binary)
        Nothing -> Prelude.error "Error parsing TokenName"
   in (toCurrencySymbol first56, toTokenName rest)

getOrderDatum :: BL.ByteString -> Maybe OrderDatum
getOrderDatum bs = case A.decode bs of
  Nothing -> Nothing
  Just any -> pure $ unDatumResponse any

datumHashToInlineDatum :: String -> IO BL.ByteString
datumHashToInlineDatum dh = do
  manager <- newTlsManager
  let url = "https://cardano-mainnet.blockfrost.io/api/v0/scripts/datum/" ++ dh
  request <- HTTP.parseRequest url
  let requestWithHeader = setRequestHeaders [("project_id", token)] request
  response <- httpLbs requestWithHeader manager
  return $ HTTP.responseBody response
  where
    token = unsafePerformIO $ BS8.readFile $ unsafePerformIO getCurrentDirectory ++ "/secrets/" ++ "blockfrost.mainnet.token"

datumHashToInlineDatumSync :: String -> BL.ByteString
datumHashToInlineDatumSync dh =
  unsafePerformIO $ datumHashToInlineDatum dh