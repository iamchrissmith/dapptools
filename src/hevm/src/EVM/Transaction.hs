{-# Language TemplateHaskell #-}
module EVM.Transaction where

import EVM.Keccak (keccak, word256Bytes, rlpWord160, rlpWord256, rlpBytes, rlpList)
import EVM.Precompiled (execute)
import EVM.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Control.Lens

data Transaction = Transaction
  { _calldata :: ByteString,
    _gasLimit :: W256,
    _gasPrice :: W256,
    _nonce    :: W256,
    _r        :: W256,
    _s        :: W256,
    _toAddr   :: Addr,
    _v        :: W256,
    _value    :: W256
  } deriving Show

makeLenses ''Transaction

ecrec :: W256 -> W256 -> W256 -> W256 -> Maybe Addr
ecrec hash v r s = (num . word) <$> EVM.Precompiled.execute 1 input 32
  where input = BS.concat $ (word256Bytes <$> [hash, v, r, s])

sender :: Transaction -> Maybe Addr
sender tx = ecrec (keccak encodedData) (view v tx) (view r tx) (view s tx)
  where encodedData = rlpList [rlpWord256 (view nonce tx),
                               rlpWord256 (view gasPrice tx),
                               rlpWord256 (view gasLimit tx),
                               rlpWord160 (view toAddr tx),
                               rlpWord256 (view value tx),
                               rlpBytes   (view calldata tx)]

testing :: Transaction -> W256
testing tx = view gasPrice tx
