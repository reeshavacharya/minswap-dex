{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MatchUtils where

import Blockfrost.Client (Amount, MonadBlockfrost, TxHash)
import qualified Debug.Trace as Debug
import OrderUtils
import Plutus.V1.Ledger.Value (AssetClass, Value, adaSymbol, adaToken, assetClass, assetClassValueOf, flattenValue)
import PlutusTx.Prelude (divide, mapMaybe, trace, traceError)
import PoolUtils
import Types

seiOrderMatchers :: [OrderMatcher] -> [OrderMatcher]
seiOrderMatchers =
  mapMaybe
    ( \x -> case x of
        OrderMatcher od va x1 -> case od of
          OrderDatum ad ad' m_dh os n i -> case os of
            SwapExactIn ac j -> Just x
            _ -> Nothing
    )

makeMatchesForSEIOrders :: Monad m => OrderMatcher -> [PoolMatcher] -> m [PoolMatches]
makeMatchesForSEIOrders orderMatcher poolMatcherList = do
  let desiredCoinAndAmount = case orderMatcher of
        OrderMatcher od va th -> case od of
          OrderDatum ad ad' m_dh os n i -> case os of
            SwapExactIn ac j -> (ac, j)
            _ -> error "impossible. If this error occurs, the orders are not filtered."
      offeredCoinAndAmount = head (getAssetClassAndAmountFromValue $ omOfferedAmount orderMatcher)
      validPool = validatePool poolMatcherList
      poolMatches =
        mapMaybe
          ( \(PoolMatcher pd val th) ->
              if (assetClassValueOf val (fst offeredCoinAndAmount) >= 1) && (assetClassValueOf val (fst desiredCoinAndAmount) >= 1)
                then
                  Just
                    ( PoolMatches
                        (fst desiredCoinAndAmount, assetClassValueOf val (fst desiredCoinAndAmount))
                        (fst offeredCoinAndAmount, assetClassValueOf val (fst offeredCoinAndAmount))
                        th
                        pd
                        val
                    )
                else Nothing
          )
          validPool
      approxDesiredCoinAmountAndPoolMatches =
        map (\x -> calculateSwapAmountOut x (snd offeredCoinAndAmount)) poolMatches
      minimumReceive = case odStep $ omOrderDatum orderMatcher of
        SwapExactIn ac n -> n
        _ -> error "impossible. If this error occurs, the orders are not filtered."
      offeredUtxo = case orderMatcher of OrderMatcher od va th -> th
      desiredUtxo = map snd approxDesiredCoinAmountAndPoolMatches
      result =
        mapMaybe
          ( \(approxDesiredCoinAmount, validPoolMatches) ->
              if approxDesiredCoinAmount >= minimumReceive
                then Just validPoolMatches
                else Nothing
          )
          approxDesiredCoinAmountAndPoolMatches
  return result

-- validatePool :: [PoolMatcher] -> [PoolMatcher]
validatePool :: [PoolMatcher] -> [PoolMatcher]
validatePool =
  mapMaybe
    ( \(PoolMatcher pd val th) -> case pd of
        PoolDatum ac ac' n i m_ps ->
          if (assetClassValueOf val ac > 0) && (assetClassValueOf val ac' > 0)
            then Just $ PoolMatcher pd val th
            else Nothing
    )

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
calculateSwapAmountOut poolMatches offeredCoinAmount =
  let offeredCoinAmountWithFee = offeredCoinAmount * 997
      numerator = offeredCoinAmountWithFee * snd (pmmDesired poolMatches)
      denominator = snd (pmmOffered poolMatches) * 1000 + offeredCoinAmountWithFee
      outAmount = numerator `divide` denominator
   in (outAmount, poolMatches)

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
