module EVM.Transaction where

import Prelude hiding (Word)

import EVM.Concrete
import EVM.FeeSchedule
import EVM.Keccak (keccak, word256Bytes, rlpWord160, rlpWord256, rlpBytes, rlpList)
import EVM.Precompiled (execute)
import EVM.Types

import Data.Aeson (FromJSON (..))
import Data.ByteString (ByteString)

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString   as BS

data Transaction = Transaction
  { txData     :: ByteString,
    txGasLimit :: W256,
    txGasPrice :: W256,
    txNonce    :: W256,
    txR        :: W256,
    txS        :: W256,
    txToAddr   :: Addr,
    txV        :: W256,
    txValue    :: W256
  } deriving Show

ecrec :: W256 -> W256 -> W256 -> W256 -> Maybe Addr
ecrec e v r s = (num . word) <$> EVM.Precompiled.execute 1 input 32
  where input = BS.concat $ (word256Bytes <$> [e, v, r, s])

sender :: Int -> Transaction -> Maybe Addr
sender chainId tx = ecrec hash v' (txR tx) (txS tx)
  where v    = txV tx
        hash = keccak $ signingData chainId tx
        v'   = if v == 27 || v == 28 then v
               else 28 - mod v 2

signingData :: Int -> Transaction -> ByteString
signingData chainId tx =
  if v == (chainId * 2 + 35) || v == (chainId * 2 + 36)
  then eip155Data
  else normalData
  where v          = fromIntegral (txV tx)
        normalData = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 (txGasPrice tx),
                              rlpWord256 (txGasLimit tx),
                              rlpWord160 (txToAddr tx),
                              rlpWord256 (txValue tx),
                              rlpBytes   (txData tx)]
        eip155Data = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 (txGasPrice tx),
                              rlpWord256 (txGasLimit tx),
                              rlpWord160 (txToAddr tx),
                              rlpWord256 (txValue tx),
                              rlpBytes   (txData tx),
                              rlpWord256 (fromIntegral chainId),
                              rlpWord256 0x0,
                              rlpWord256 0x0]

txGasCost :: FeeSchedule Word -> Transaction -> Word
txGasCost fs tx =
  let calldata     = txData tx
      zeroBytes    = BS.count 0 calldata
      nonZeroBytes = BS.length calldata - zeroBytes
      baseCost     = g_transaction fs
        + if txToAddr tx == 0 then g_txcreate fs else 0
      zeroCost     = g_txdatazero fs
      nonZeroCost  = g_txdatanonzero fs
  in baseCost + zeroCost * (fromIntegral zeroBytes) + nonZeroCost * (fromIntegral nonZeroBytes)

instance FromJSON Transaction where
  parseJSON (JSON.Object val) = do
    tdata    <- dataField val "data"
    gasLimit <- wordField val "gasLimit"
    gasPrice <- wordField val "gasPrice"
    nonce    <- wordField val "nonce"
    r        <- wordField val "r"
    s        <- wordField val "s"
    toAddr   <- addrField val "to"
    v        <- wordField val "v"
    value    <- wordField val "value"
    return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value
  parseJSON invalid =
    JSON.typeMismatch "Transaction" invalid

badtx1 = Transaction {txData = "a\ETX\232`dR`\NUL`\NUL` `\NUL`\NULs\176\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULb\EOT\147\224\242P`\NUL`\NUL`\NUL`\NULs0\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULb\EOT\147\224\244P`\NUL`\NUL` `\NULs\176\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULb\EOT\147\224\244P`\NUL`\NUL`\NUL`\NUL`\NULs\DLE\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULb\EOT\147\224\242P`\NUL`\NUL`\NUL`\NUL`\NULs \NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NULb\t'\192\241", txGasLimit = 0x1e8480, txGasPrice = 0x1, txNonce = 0x0, txR = 0xcee9dea4248d515aeadc1dc546cb6d0fbf71a92576b53138c84aef648fccc619, txS = 0xf95417ec51f5dc156685d227b306baf914f88d17b5ddf5ab9080399186bc68, txToAddr = 000000000000, txV = 0x1b, txValue = 0x1}
-- sender 1 badtx1 = 0ffc07e42e53c5ad65c281de491d0363a0427456
-- sender should be a94f5374fce5edbc8e2a8697c15331677e6ebf0b
-- PROBABLY because of large data
