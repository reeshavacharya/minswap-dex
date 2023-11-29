{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MatchUtils where

import Blockfrost.Client (Amount, MonadBlockfrost, TxHash)
import qualified Debug.Trace as Debug
import OrderUtils
import Plutus.V1.Ledger.Value (AssetClass, Value, adaSymbol, adaToken, assetClass, assetClassValueOf, flattenValue)
import PlutusTx.Prelude (divide)
import PoolUtils
import Types

makeMatchesForSEIOrders :: Monad m => OrderMatcher -> PoolMatcher -> m Match
makeMatchesForSEIOrders orderMatcher poolMatcher = do
  let desiredCoinAndAmount = case orderMatcher of
        OrderMatcher od va th -> case od of
          OrderDatum ad ad' m_dh os n i -> case os of
            SwapExactIn ac j -> (ac, j)
            _ -> error "Not an SEI order"
      offeredCoinAndAmount = head (getAssetClassAndAmountFromValue $ omOfferedAmount orderMatcher)
      validPool = validatePool poolMatcher
      desiredCoinAmountInPool = case validPool of
        PoolMatcher pd va th ->
          if assetClassValueOf va (fst desiredCoinAndAmount) >= 1
            then assetClassValueOf va (fst desiredCoinAndAmount)
            else error "No Desired Coin In Pool"
      offeredCoinAmountInPool = case validPool of
        PoolMatcher pd va th ->
          if assetClassValueOf va (fst offeredCoinAndAmount) >= 1
            then assetClassValueOf va (fst offeredCoinAndAmount)
            else error "No Offered Coin In Pool"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
      approxDesiredCoinAmount =
        calculateSwapAmountOut
          offeredCoinAmountInPool
          desiredCoinAmountInPool
          (snd offeredCoinAndAmount)
      minimumReceive = case odStep $ omOrderDatum orderMatcher of
        SwapExactIn ac n -> n
        _ -> error "Not an SEI Order"
      offeredUtxo = case orderMatcher of OrderMatcher od va th -> th
      deriredUtxo = case poolMatcher of PoolMatcher pd va th -> th
      sender = odSender (omOrderDatum orderMatcher)
      receiver = odReceiver (omOrderDatum orderMatcher)
      result =
        if approxDesiredCoinAmount >= minimumReceive
          then
            Match
              offeredCoinAndAmount
              offeredUtxo
              (fst desiredCoinAndAmount, approxDesiredCoinAmount)
              deriredUtxo
              sender
              receiver
              (omOrderDatum orderMatcher)
              (pmPoolDatum poolMatcher)
          else error "Cant Make Match"
  return result

validatePool :: PoolMatcher -> PoolMatcher
validatePool poolMatcher = case poolMatcher of
  PoolMatcher pd val th -> case pd of
    PoolDatum ac ac' n i m_ps ->
      if (assetClassValueOf val ac > 0) && (assetClassValueOf val ac' > 0)
        then poolMatcher
        else error "Invalid Pool"

poolCoinContentToTuple :: PoolCoinContent -> [(AssetClass, Integer)]
poolCoinContentToTuple pcc = [(pccCoinA pcc, pccCoinAQuantity pcc), (pccCoinB pcc, pccCoinBQuantity pcc)]

dummyOrder = [(assetClass "34e6054e3e74ecf4a9b6eb4e34a865f6b0b5fee85e6a9bfba0d2d025" "MINSWAP", 30), (assetClass adaSymbol adaToken, 30000000)]

dummyPool =
  [ [(assetClass "34e6054e3e74ecf4a9b6eb4e34a865f6b0b5fee85e6a9bfba0d2d025" "MINSWAP", 30), (assetClass adaSymbol adaToken, 10000000)],
    [(assetClass "34e6054e3e74ecf4a9b6eb4e34a865f6b0b5fee85e6a9bfba0d2d025" "MINSWAP", 30), (assetClass adaSymbol adaToken, 10000000)]
  ]

possibility = makeValidTuples dummyOrder dummyPool

-- Calculates how much asset B the trader will receive for the specified input of asset A and the current reserves of the pool.
-- Calculated based on 0.3% slippage
calculateSwapAmountOut :: Integer -> Integer -> Integer -> Integer
calculateSwapAmountOut offeredCoinAmountInPool desiredCoinAmountInPool offeredCoinAmount =
  let offeredCoinAmountWithFee = offeredCoinAmount * 997
      numerator = offeredCoinAmountWithFee * desiredCoinAmountInPool
      denominator = offeredCoinAmountInPool * 1000 + offeredCoinAmountWithFee
      outAmount = numerator `divide` denominator
   in outAmount

-- Calculates how much of asset A the trader needs to deposit to obtain a desired amount of asset B
-- calculateSwapAmountIn :: Integer -> Integer -> Integer -> Integer
-- TODO: complete this funciton

makeValidTuples :: [(AssetClass, Integer)] -> [[(AssetClass, Integer)]] -> Bool
makeValidTuples [(offerAC, offer_amt), (desiredAc, desired_amt)] poolTokens =
  or $
    foldr
      ( \x acc -> case x of
          [(poolCoinA, poolCoinA_amt), (poolCoinB, poolCoinB_amt)] ->
            if (poolCoinA, poolCoinB) == (offerAC, desiredAc)
              || (poolCoinA, poolCoinB) == (desiredAc, offerAC)
              then
                if (poolCoinA, poolCoinB) == (offerAC, desiredAc)
                  then (poolCoinB_amt >= desired_amt) : acc
                  else (poolCoinA_amt >= desired_amt) : acc
              else False : acc
          _ -> error "Impossible"
      )
      []
      poolTokens

isMatchPossible :: [(AssetClass, Integer)] -> [[(AssetClass, Integer)]] -> Bool
isMatchPossible offerAndOrderTokens poolTokens =
  parseOrdersToOnlyAsset `elem` parsePoolsToOnlyAsset
    || swapTuples parseOrdersToOnlyAsset `elem` parsePoolsToOnlyAsset
  where
    parseOrdersToOnlyAsset = case offerAndOrderTokens of
      [(offerAC, _), (orderAC, _)] -> (offerAC, orderAC)
      _ -> error "Impossible"
    parsePoolsToOnlyAsset =
      foldr
        ( \x acc -> case x of
            [(poolCoinA, _), (poolCoinB, _)] -> (poolCoinA, poolCoinB) : acc
            _ -> error "Impossible"
        )
        []
        poolTokens
    swapTuples orderTuples = case orderTuples of
      (a, b) -> (b, a)

getAssetClassAndAmountFromValue :: Value -> [(AssetClass, Integer)]
getAssetClassAndAmountFromValue val =
  let flattenedValue = flattenValue val
      integerOnly = foldr (\x acc -> case x of (cs, tn, n) -> acc + n) 0 flattenedValue
      adaValueOnly =
        foldr
          ( \x acc -> case x of
              (cs, tn, n) ->
                (cs == adaSymbol && tn == adaToken) && acc
          )
          True
          flattenedValue
      findAssetAndAmountInValue =
        foldr (\x acc -> case x of (cs, tn, n) -> if cs == adaSymbol then acc else (assetClass cs tn, n) : acc) [] flattenedValue
      returnValue = if adaValueOnly then [(assetClass adaSymbol adaToken, integerOnly)] else findAssetAndAmountInValue
   in returnValue
