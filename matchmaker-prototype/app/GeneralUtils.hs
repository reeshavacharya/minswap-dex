module GeneralUtils where 
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Api (fromBuiltin)
import qualified Data.ByteString.Char8 as BS8

parseTxId :: Plutus.TxId -> [Char]
parseTxId txid = case txid of { Plutus.TxId bbs -> BS8.unpack $ fromBuiltin bbs } 