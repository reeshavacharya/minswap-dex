{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Types where

import Blockfrost.Client (TxHash)
import Cardano.Api
import qualified Cardano.Api as CApi
import Cardano.Api.Shelley (scriptDataFromJsonDetailedSchema, scriptDataToJsonDetailedSchema, toPlutusData)
import Cardano.Crypto.Hash (ByteString)
import Cardano.Kuber.Data.Parsers
import Cardano.Kuber.Util
import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Functor
import qualified Data.Text as T
import GHC.Base
import GHC.Generics
import GHC.Show
import Money
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value
import PlutusTx hiding (txOutDatum)
import PlutusTx.Prelude hiding ((++), (<$>), (<*>))
import qualified Prelude

-- Data Types

data Match = Match
  { mOffered :: (AssetClass, Integer),
    mOfferedUtxo :: (String, Integer),
    mDesired :: (AssetClass, Integer),
    mDesiredUtxo :: (String, Integer),
    mSender :: Plutus.Address,
    mReceiver :: Plutus.Address,
    mOrderDatum :: OrderDatum,
    mPoolDatum :: PoolDatum
  }
  deriving (Show)

data PoolCoinContent = PoolCoinContent
  { pccCoinA :: AssetClass,
    pccCoinAQuantity :: Integer,
    pccCoinB :: AssetClass,
    pccCoinBQuantity :: Integer,
    uTxo :: TxHash
  }
  deriving (Show)

data OrderStep
  = SwapExactIn
      { seiDesiredCoin :: AssetClass,
        seiMinimumReceive :: Integer
      }
  | SwapExactOut
      { seoDesiredCoin :: AssetClass,
        seoExpectedReceive :: Integer
      }
  | Deposit
      { dMinimumLP :: Integer
      }
  | Withdraw
      { wMinimumCoinA :: Integer,
        wMinimumCoinB :: Integer
      }
  | OneSideDeposit
      { osdDesiredCoin :: AssetClass,
        osdMinimumLP :: Integer
      }
  deriving (Show)

data OrderMatcher = OrderMatcher
  { omOrderDatum :: OrderDatum,
    omOfferedAmount :: Plutus.V1.Ledger.Value.Value,
    orderUtxo :: (String, Integer)
  }
  deriving (Show)

data PoolMatcher = PoolMatcher
  { pmPoolDatum :: PoolDatum,
    pmValue :: Plutus.V1.Ledger.Value.Value,
    poolUtxo :: (String, Integer)
  }
  deriving (Show)

data OrderDatum = OrderDatum
  { odSender :: Plutus.V1.Ledger.Api.Address,
    odReceiver :: Plutus.V1.Ledger.Api.Address,
    odReceiverDatumHash :: Maybe DatumHash,
    odStep :: OrderStep,
    odBatcherFee :: Integer,
    odOutputADA :: Integer
  }
  deriving (Show)

newtype ApiDatumResponse' = ApiDatumResponse' BuiltinData

newtype ApiDatumResponse a = ApiDatumResponse a

data PoolDatum = PoolDatum
  { pdCoinA :: AssetClass,
    pdCoinB :: AssetClass,
    pdTotalLiquidity :: Integer,
    pdRootKLast :: Integer,
    pdProfitSharing :: Maybe ProfitSharing
  }
  deriving (Show)

data ProfitSharing = ProfitSharing
  { psFeeTo :: Plutus.V1.Ledger.Api.Address,
    psFeeToDatumHash :: Maybe DatumHash
  }
  deriving (Show)

-- PlutusTx Instances

PlutusTx.makeIsDataIndexed ''ProfitSharing [('ProfitSharing, 0)]
PlutusTx.makeLift ''ProfitSharing

PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]
PlutusTx.makeLift ''PoolDatum

PlutusTx.makeIsDataIndexed
  ''OrderStep
  [ ('SwapExactIn, 0),
    ('SwapExactOut, 1),
    ('Deposit, 2),
    ('Withdraw, 3),
    ('OneSideDeposit, 4)
  ]
PlutusTx.makeLift ''OrderStep

PlutusTx.makeIsDataIndexed ''OrderDatum [('OrderDatum, 0)]
PlutusTx.makeLift ''OrderDatum

data OrderRedeemer = ApplyOrder | CancelOrder

PlutusTx.makeIsDataIndexed
  ''OrderRedeemer
  [ ('ApplyOrder, 0),
    ('CancelOrder, 1)
  ]
PlutusTx.makeLift ''OrderRedeemer

-- FromData Instances
unDatumResponse (ApiDatumResponse a) = a

instance FromData a => FromJSON (ApiDatumResponse a) where
  parseJSON v@(A.Object o) = do
    (ApiDatumResponse' btinData) <- parseJSON v
    case fromBuiltinData btinData of
      Nothing -> fail "Failed to convert parsed data to required type"
      Just any -> GHC.Base.pure (ApiDatumResponse any)
  parseJSON _ = fail "Expected ApiDatumResponse Object"

instance FromJSON ApiDatumResponse' where
  parseJSON (A.Object o) = do
    scriptDataJson <- o .: "json_value"
    case scriptDataFromJsonDetailedSchema scriptDataJson of
      Left sdjse -> fail ("Invalid ScritpDataJson :" ++ show sdjse)
      Right sd -> GHC.Base.pure (ApiDatumResponse' (dataToBuiltinData (toPlutusData sd)))
  parseJSON _ = fail "Expected ApiDatumResponse Object"

-- FromJSON Instances

instance FromJSON Plutus.V1.Ledger.Api.Address where
  parseJSON (A.String s) = case deserialiseAddress (AsAddressInEra AsBabbageEra) s of
    Nothing -> fail "Invalid"
    Just aie -> GHC.Base.pure (addrInEraToPlutusAddress aie)
  parseJSON _ = fail "Invalid"

instance FromJSON DatumHash where
  parseJSON (A.String s) = case unHex s of
    Just (binary :: BS.ByteString) -> GHC.Base.pure (DatumHash (toBuiltin binary))
    Nothing -> Prelude.error "Error parsing PKH"
  parseJSON _ = fail "Invalid"

instance FromJSON AssetClass where
  parseJSON (A.String s) = case parseAssetId s of
    Just a -> GHC.Base.pure (toPlutusAssetClass a)
    Nothing -> fail "Invalid"
  parseJSON _ = fail "Invalid"

instance FromJSON OrderStep where
  parseJSON = withObject "OrderStep" GHC.Base.$ \obj -> do
    tag <- obj .: "tag"
    case tag :: String of
      "SwapExactIn" -> SwapExactIn <$> obj .: "seiDesiredCoin" <*> obj .: "seiMinimumReceive"
      "SwapExactOut" -> SwapExactOut <$> obj .: "seoDesiredCoin" <*> obj .: "seoExpectedReceive"
      "Deposit" -> Deposit <$> obj .: "dMinimumLP"
      "Withdraw" -> Withdraw <$> obj .: "wMinimumCoinA" <*> obj .: "wMinimumCoinB"
      "OneSideDeposit" -> OneSideDeposit <$> obj .: "osdDesiredCoin" <*> obj .: "osdMinimumLP"
      _ -> fail "Invalid tag for OrderStep"

instance FromJSON OrderDatum where
  parseJSON :: Data.Aeson.Types.Value -> Parser OrderDatum
  parseJSON (A.Object o) =
    do
      OrderDatum
      <$> o .: "odSender"
      <*> o .: "odReceiver"
      <*> o .: "odReceiverDatumHash"
      <*> o .: "odStep"
      <*> o .: "odBatcherFee"
      <*> o .: "odOutputADA"
  parseJSON _ = fail "Invalid"

instance FromJSON ByteString where
  parseJSON (A.String o) = case unHex o of
    Just (binary :: BS.ByteString) -> GHC.Base.pure binary
    Nothing -> fail "Invalid"
  parseJSON _ = fail "Invalid"