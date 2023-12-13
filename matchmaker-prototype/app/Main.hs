{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Api (AssetId (AdaAssetId), NetworkId (Testnet), NetworkMagic (NetworkMagic), SerialiseAddress (serialiseAddress), UTxO (UTxO), getTxBody, getTxId, prettyPrintJSON, renderValue, valueFromList)
import Cardano.Kuber.Api
import Cardano.Kuber.Console.ConsoleWritable (ConsoleWritable (toConsoleText, toConsoleTextNoPrefix))
import Cardano.Kuber.Util (addressInEraToAddressAny, fromPlutusAddress)
import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Debug.Trace as Debug
import GeneralUtils
import Hedgehog.Gen (sample)
import MatchUtils
import OrderUtils
import Plutus.V1.Ledger.Value (TokenName (TokenName), assetClass, assetClassValue)
import PlutusTx.Prelude (divide, isJust, traceError)
import PoolUtils
import TransactionMaker
import Types

showMatches :: PoolMatches -> IO ()
showMatches matches = do
  putStrLn $ "TxId    : " ++ show (fst (pmmDesiredUtxo matches) ++ "#" ++ show (snd (pmmDesiredUtxo matches)))
  putStrLn $ "CoinA   :" ++ show (snd $ pmmDesired matches) ++ " " ++ show (fst $ pmmDesired matches)
  putStrLn $ "CoinB   :" ++ show (snd $ pmmOffered matches) ++ " " ++ show (fst $ pmmOffered matches)
  putStrLn $ "\n\n"

showOrders :: OrderMatcher -> IO ()
showOrders odMatcher = do
  putStrLn $ "TxId    : " ++ show (fst (omOrderUtxo odMatcher) ++ "#" ++ show (snd (omOrderUtxo odMatcher)))
  putStrLn $ "Desired : " ++ T.unpack (renderValue $ fromPlutusValue (assetClassValue (seiDesiredCoin $ odStep $ omOrderDatum odMatcher) (seiMinimumReceive $ odStep $ omOrderDatum odMatcher)))
  putStrLn $ "Offered : " ++ T.unpack (renderValue $ fromPlutusValue (omOfferedAmount odMatcher))
  putStrLn $ "From    : " ++ T.unpack (plutusAddrToText $ odSender $ omOrderDatum odMatcher)
  putStrLn $ "To      : " ++ T.unpack (plutusAddrToText $ odReceiver $ omOrderDatum odMatcher)
  putStrLn $ "Fee     : " ++ show (odBatcherFee $ omOrderDatum odMatcher)
  putStrLn "\n\n"

dummyOrderMatcher :: OrderMatcher
dummyOrderMatcher = OrderMatcher od val ("586108e5792f738bc1cf91fe5a18fff88425eefbc4d219f417bb00d87a9f0840", 1)
  where
    someAddr = parseToPlutusAddress "addr1q80k5uczwvzdt6g6c2e9l92r24f528dfnmu9qq6lssrc3z56l3lczn30u56k4vlf948etel5d63zj20yg6wymu2gp4gs82x3ua"
    od = OrderDatum someAddr someAddr Nothing odStep 3000000 3000000
    odStep = SwapExactIn (assetClass "48c5dbb9cf08baccdcf33d3e45add745491214724d4782ccc99cf7f3" "VEEV") 10
    val = toPlutusValue $ valueFromList [(AdaAssetId, 20000000)]

remoteKuberConnection :: IO RemoteKuberConnection
remoteKuberConnection = do
  (networkName, network) <- getNetworkFromEnv "NETWORK"
  createRemoteKuberConnection network "http://172.31.6.14:8081/" Nothing

main :: IO ()
main = do
  let order = dummyOrderMatcher
  chainInfo <- remoteKuberConnection
  poolMatcher <- getUtxosWithPoolTokens
  time <- getTime
  matchingPool <- makeMatchesForSEIOrders order poolMatcher
  let pool = if null matchingPool then error "No matches exist for your order" else head matchingPool
  poolScrpt <- poolScript
  orderScrpt <- orderScript
  queryBatcher <- evaluateKontract chainInfo do getbatcherUtxo'
  batcherUtxo <- case queryBatcher of
    Left fe -> error "no valid batcher utxo to consume"
    Right uto -> case uto of UTxO map -> pure $ head $ Map.toList map
  tx <- makeSeiTx order orderScrpt pool poolScrpt time batcherUtxo
  Debug.traceM (BS8.unpack $ prettyPrintJSON tx)
  result <- evaluateKontract chainInfo $ kBuildAndSubmit tx
  putStrLn "\n\n\n"
  case result of
    Left fe -> print $ show fe
    Right tx' -> print $ getTxId $ getTxBody tx'

-- for viewing matches for `dummyOrderMatcher`
-- main :: IO ()
-- main = do
--   -- odMatcher <- generateOrderMatcher -- query SEI orders from order contract
--   let odMatcher = [dummyOrderMatcher]
--   putStrLn "\n ---------------------------------------------------------------------------------------------------------------------------------- \n"
--   putStrLn $ "Order:  \n"
--   mapM_ showOrders (seiOrderMatchers odMatcher)
--   pMatcher <- getUtxosWithPoolTokens
--   putStrLn "\n ---------------------------------------------------------------------------------------------------------------------------------- \n"
--   matches <- makeMatchesForSEIOrders (head odMatcher) pMatcher
--   putStrLn $ "Matches:  \n"
--   mapM_ showMatches matches
