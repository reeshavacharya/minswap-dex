-- ADA-AGIX pool: addr1z8snz7c4974vzdpxu65ruphl3zjdvtxw8strf2c2tmqnxzfgf0jgfz5xdvg2pges20usxhw8zwnkggheqrxwmxd6huuqss46eh
-- addr1z8snz7c4974vzdpxu65ruphl3zjdvtxw8strf2c2tmqnxzfpahn0eymy6pz4yr4mvptlpjv9eh2swz0u0hjj4qp439kq4064e4 -- JPG POOL
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PoolUtils where

import Blockfrost.Client
import Blockfrost.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Money
import OrderUtils
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import qualified Plutus.V1.Ledger.Value as Value
import Types
import GeneralUtils

poolAddressInfo :: MonadBlockfrost m => m [AddressUtxo]
poolAddressInfo = do
  getAddressUtxos (Blockfrost.Types.Address (T.pack "addr_test1zqf5dxz2jma5a8vpyvxa4v4qq67xp2kzkhmgeud3j4ul8ujj2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pqcscau3"))

minswapPolicy :: CurrencySymbol
minswapPolicy = "aef680e98237d089f5ed465298065536aab01b2a92c813f590f91b58"

minswapTokenName :: TokenName
minswapTokenName = "MINSWAP"

poolToken = assetClass minswapPolicy minswapTokenName

amountToAssetClass :: [Amount] -> [AssetClass]
amountToAssetClass =
  foldr
    ( \x acc -> case x of
        AdaAmount dis ->
          assetClass adaSymbol adaToken :
          acc
        AssetAmount sd -> uncurry assetClass (splitText $ someDiscreteCurrency sd) : acc
    )
    []

getUtxosWithPoolTokens :: (MonadBlockfrost m) => m [PoolMatcher]
getUtxosWithPoolTokens = do
  addressUtxos <- poolAddressInfo
  let txHashAnddatum =
        foldr
          ( \x acc -> case x of
              AddressUtxo ad th n ams bh m_dh m_id m_sh -> case m_dh of
                Nothing -> acc
                Just dh -> case dh of
                  Blockfrost.Types.DatumHash txt -> (txHashtoTxOut th n, datumHashToInlineDatumSync (T.unpack txt), mconcat (amountToValue ams)) : acc
          )
          []
          (filterUtxosWithAssets poolToken addressUtxos)
  return $
    foldr
      ( \x acc -> case x of
          (th, bs, val) -> case getPoolDatum bs of
            Nothing -> acc
            Just pd -> PoolMatcher pd val (case th of TxOutRef ti n -> (parseTxId ti, n)) : acc
      )
      []
      txHashAnddatum

filterTokenandAdaUtxos :: AddressUtxo -> (TxHash, Value)
filterTokenandAdaUtxos addrUtxos = case addrUtxos of
  AddressUtxo ad th n ams bh m_dh m_id m_sh -> (th, mconcat $ getThValue ams)
  where
    getThValue ams =
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
        ams
    getAssetClass sd = splitText $ someDiscreteCurrency sd

filterDatum :: AddressUtxo -> (TxHash, T.Text)
filterDatum addrUtxos = case addrUtxos of
  AddressUtxo ad th n ams bh m_dh m_id m_sh -> (th, getInlineDatum m_dh)
  where
    getInlineDatum m_dh = case m_dh of
      Just dh -> case dh of
        Blockfrost.Types.DatumHash txt -> txt
      Nothing -> error "Invalid dat"

filterUtxosWithAssets :: AssetClass -> [AddressUtxo] -> [AddressUtxo]
filterUtxosWithAssets ac =
  foldr
    ( \x acc -> case x of
        AddressUtxo ad th n ams bh m_dh m_id m_sh ->
          if ac `elem` amountToAssetClass ams
            then x : acc
            else acc
    )
    []

-- getPoolCoinPair :: MonadBlockfrost m => m [PoolCoinContent]
-- getPoolCoinPair = do
--   foldr
--     ( \x acc -> case x of
--         (th, pd', val) ->
--           if (pdCoinA pd' `elem` flattenedValue val) && (pdCoinB pd' `elem` flattenedValue val)
--             then
--               PoolCoinContent
--                 (pdCoinA pd')
--                 (findQuantity val (pdCoinA pd'))
--                 (pdCoinB pd')
--                 (findQuantity val (pdCoinB pd'))
--                 th :
--               acc
--             else acc
--     )
--     []
--     <$> getUtxosWithPoolTokens
--   where
--     flattenedValue v = map (\(cs, tn, int) -> assetClass cs tn) (Value.flattenValue v)
--     findQuantity val ac = assetClassValueOf val ac

getPoolDatum :: BL.ByteString -> Maybe PoolDatum
getPoolDatum bs = case A.decode bs of
  Nothing -> Nothing
  Just any -> pure $ unDatumResponse any