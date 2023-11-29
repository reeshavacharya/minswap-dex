{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Kuber.Api
import qualified Data.Aeson as A
import qualified Debug.Trace as Debug
import MatchUtils
import OrderUtils
import Plutus.V1.Ledger.Value (assetClassValue)
import PlutusTx.Prelude (divide, traceError)
import PoolUtils
import TransactionMaker

calculateInitialLiquidity :: Integer -> Integer -> Integer
calculateInitialLiquidity outA outB =
  let p = outA * outB
      sqrt = calSqrt p
   in if sqrt * sqrt < p
        then sqrt + 1
        else sqrt

calSqrt :: Integer -> Integer
calSqrt x
  | x < 0 = traceError "negative square root condition from factory token contract"
  | x == 0 = 0
  | x == 1 = 1
  | x == 2 = 1
  | otherwise = go x (x `divide` 2 + 1)
  where
    go :: Integer -> Integer -> Integer
    go i1 i2 =
      if i2 < i1
        then go i2 ((x `divide` i2 + i2) `divide` 2)
        else i1


-- main = print $ calculateInitialLiquidity 3000000 33 

main :: IO ()
main = do
  odMatcher <- parsedDatumInline
  print $ head odMatcher
  pMatcher <- getUtxosWithPoolTokens
  print "\n ----------------------------------------------------------------- \n"
  print $ head pMatcher
  printThis <- makeMatchesForSEIOrders (head odMatcher) (head pMatcher)
  print printThis

-- print (calculateInitialLiquidity 10000000 10000002)
