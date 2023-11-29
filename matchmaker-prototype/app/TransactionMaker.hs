{-# LANGUAGE OverloadedStrings #-}

module TransactionMaker where

import Cardano.Kuber.Api (TxBuilder)
import Cardano.Kuber.Util
import Plutus.V1.Ledger.Api
import PlutusTx.Builtins (emptyByteString, sha2_256)
import PlutusTx.Prelude (consByteString, quotient, remainder)
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
    tokenName = TokenName $ sha2_256 $ getTxId refHash <> integerToBS refIdx

myTokenName = mkNFTTokenName (TxOutRef (TxId "4933959fb4545eaf39e4bed18b10fc39d71e18385f860114b48e040319c5c334") 1)

-- makeSeiTransaction :: Match -> TxBuilder
-- makeSeiTransaction match = do