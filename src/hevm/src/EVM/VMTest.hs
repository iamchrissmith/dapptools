{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.VMTest
  ( Case
#if MIN_VERSION_aeson(1, 0, 0)
  , parseSuite
  , parseBCSuite
#endif
  , vmForCase
  , checkExpectation
  , interpret
  ) where

import qualified EVM
import qualified EVM.Concrete as EVM
import qualified EVM.Exec
import qualified EVM.FeeSchedule as EVM.FeeSchedule
import qualified EVM.Stepper as Stepper
import qualified EVM.Fetch as Fetch

import Control.Monad.State.Strict (runState, join)
import qualified Control.Monad.Operational as Operational
import qualified Control.Monad.State.Class as State

import EVM (EVM)
import EVM.Stepper (Stepper)
import EVM.Transaction
import EVM.Types

import Control.Lens

import IPPrint.Colored (cpprint)

import Data.ByteString (ByteString)
import Data.Aeson ((.:), (.:?))
import Data.Aeson (FromJSON (..))
import Data.Either (isLeft)
import Data.Map (Map)
import Data.List (intercalate)

import qualified Data.Map          as Map
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy  as Lazy

data Which = Pre | Post

data Block = Block
  { blockCoinbase   :: Addr
  , blockDifficulty :: W256
  , blockGasLimit   :: W256
  , blockNumber     :: W256
  , blockTimestamp  :: W256
  , blockTxs        :: [Transaction]
  } deriving Show

data Case = Case
  { testVmOpts      :: EVM.VMOpts
  , testContracts   :: Map Addr Contract
  , testExpectation :: Maybe Expectation
  } deriving Show

data BlockchainCase = BlockchainCase
  { blockchainBlocks  :: [Block]
  , blockchainPre     :: Map Addr Contract
  , blockchainPost    :: Map Addr Contract
  , blockchainNetwork :: String
  } deriving Show

data Contract = Contract
  { _balance :: W256
  , _code    :: ByteString
  , _nonce   :: W256
  , _storage :: Map W256 W256
  } deriving Show

data Expectation = Expectation
  { expectedOut       :: Maybe ByteString
  , expectedContracts :: Map Addr Contract
  , expectedGas       :: Maybe W256
  } deriving Show

makeLenses ''Contract

checkExpectation :: Case -> EVM.VM -> IO Bool
checkExpectation x vm =
  case (testExpectation x, view EVM.result vm) of
    (Just expectation, Just (EVM.VMSuccess output)) -> do
      let
        (s1, b1) = ("bad-state", checkExpectedContracts vm (expectedContracts expectation))
        (s2, b2) = ("bad-output", checkExpectedOut output (expectedOut expectation))
        (s3, b3) = ("bad-gas", checkExpectedGas vm (expectedGas expectation))
        ss = map fst (filter (not . snd) [(s1, b1), (s2, b2), (s3, b3)])
      if not (b1 && b2 && b3) then do
        putStr (intercalate " " ss)
        putStrLn ""
        putStrLn "Expected postState: "
        putStrLn $ show (expectedContracts expectation)
        putStrLn "Actual postState: "
        putStrLn $ show (vm ^. EVM.env . EVM.contracts . to (fmap clearZeroStorage))
        else do return ()
      return (b1 && b2 && b3)
    (Nothing, Just (EVM.VMSuccess _)) -> do
      putStr "unexpected-success"
      return False
    (Nothing, Just (EVM.VMFailure _)) ->
      return True
    (Just _, Just (EVM.VMFailure _)) -> do
      putStr "unexpected-failure"
      return False
    (_, Nothing) -> do
      cpprint (view EVM.result vm)
      error "internal error"

checkExpectedOut :: ByteString -> Maybe ByteString -> Bool
checkExpectedOut output ex = case ex of
  Nothing       -> True
  Just expected -> output == expected

checkExpectedContracts :: EVM.VM -> Map Addr Contract -> Bool
checkExpectedContracts vm expected =
  realizeContracts expected == vm ^. EVM.env . EVM.contracts . to (fmap clearZeroStorage)

clearZeroStorage :: EVM.Contract -> EVM.Contract
clearZeroStorage =
  over EVM.storage (Map.filterWithKey (\_ x -> x /= 0))

checkExpectedGas :: EVM.VM -> Maybe W256 -> Bool
checkExpectedGas vm ex = case ex of
  Nothing -> True
  Just expected -> case vm ^. EVM.state . EVM.gas of
    EVM.C _ x | x == expected -> True
    _ -> False

#if MIN_VERSION_aeson(1, 0, 0)

instance FromJSON Contract where
  parseJSON (JSON.Object v) = Contract
    <$> v .: "balance"
    <*> (hexText <$> v .: "code")
    <*> v .: "nonce"
    <*> v .: "storage"
  parseJSON invalid =
    JSON.typeMismatch "VM test case contract" invalid

instance FromJSON Case where
  parseJSON (JSON.Object v) = Case
    <$> parseVmOpts v
    <*> parseContracts Pre v
    <*> parseExpectation v
  parseJSON invalid =
    JSON.typeMismatch "VM test case" invalid

instance FromJSON BlockchainCase where
  parseJSON (JSON.Object v) = BlockchainCase
    <$> v .: "blocks"
    <*> parseContracts Pre v
    <*> parseContracts Post v
    <*> v .: "network"
  parseJSON invalid =
    JSON.typeMismatch "GeneralState test case" invalid

instance FromJSON Block where
  parseJSON (JSON.Object v) = do
    v'         <- v .: "blockHeader"
    txs        <- v .: "transactions"
    coinbase   <- addrField v' "coinbase"
    difficulty <- wordField v' "difficulty"
    gasLimit   <- wordField v' "gasLimit"
    number     <- wordField v' "number"
    timestamp  <- wordField v' "timestamp"
    return $ Block coinbase difficulty gasLimit number timestamp txs
  parseJSON invalid =
    JSON.typeMismatch "Block" invalid

parseVmOpts :: JSON.Object -> JSON.Parser EVM.VMOpts
parseVmOpts v =
  do envV  <- v .: "env"
     execV <- v .: "exec"
     case (envV, execV) of
       (JSON.Object env, JSON.Object exec) ->
         EVM.VMOpts
           <$> dataField exec "code"
           <*> dataField exec "data"
           <*> wordField exec "value"
           <*> addrField exec "address"
           <*> addrField exec "caller"
           <*> addrField exec "origin"
           <*> wordField exec "gas" -- XXX: correct?
           <*> wordField exec "gas" -- XXX: correct?
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentDifficulty"
           <*> wordField env  "currentGasLimit"
           <*> wordField exec "gasPrice"
           <*> pure (EVM.FeeSchedule.homestead)
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseContracts ::
  Which -> JSON.Object -> JSON.Parser (Map Addr Contract)
parseContracts w v =
  v .: which >>= parseJSON
  where which = case w of
          Pre  -> "pre"
          Post -> "postState"

parseExpectation :: JSON.Object -> JSON.Parser (Maybe Expectation)
parseExpectation v =
  do out       <- fmap hexText <$> v .:? "out"
     contracts <- v .:? "post"
     gas       <- v .:? "gas"
     case (out, contracts, gas) of
       (Just x, Just y, Just z) ->
         return (Just (Expectation (Just x) y (Just z)))
       _ ->
         return Nothing

parseSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseSuite = JSON.eitherDecode'

parseBCSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseBCSuite x = case (JSON.eitherDecode' x) :: Either String (Map String BlockchainCase) of
  Left e        -> Left e
  Right bcCases -> let allCases = (fromBlockchainCase <$> bcCases)
                       rightNetwork (Left OldNetwork) = False
                       rightNetwork _                 = True
                       rightNetworkCases = Map.filter rightNetwork allCases
                       rightToMaybe (Left _)  = Nothing
                       rightToMaybe (Right b) = Just b
    in case sequence (rightToMaybe <$> rightNetworkCases) of
    Just cases -> Right cases
    Nothing    -> case (Map.elems (Map.filter isLeft rightNetworkCases) !! 0) of
      Left e -> Left (show e)
#endif

realizeContracts :: Map Addr Contract -> Map Addr EVM.Contract
realizeContracts = Map.fromList . map f . Map.toList
  where
    f (a, x) = (a, realizeContract x)

realizeContract :: Contract -> EVM.Contract
realizeContract x =
  EVM.initialContract (EVM.RuntimeCode (x ^. code))
    & EVM.balance .~ EVM.w256 (x ^. balance)
    & EVM.nonce   .~ EVM.w256 (x ^. nonce)
    & EVM.storage .~ (
        Map.fromList .
        map (\(k, v) -> (EVM.w256 k, EVM.w256 v)) .
        Map.toList $ x ^. storage
        )

data BlockchainError = TooManyBlocks | TooManyTxs | CreationUnsupported | TargetMissing | SignatureUnverified | InvalidTx | OldNetwork deriving Show

fromBlockchainCase :: BlockchainCase -> Either BlockchainError Case
fromBlockchainCase (BlockchainCase blocks preState postState network) =
  case (blocks, network) of
    ((block : []), "ConstantinopleFix") -> case blockTxs block of
      (tx : []) -> fromGoodBlockchainCase block tx preState postState
      _         -> Left TooManyTxs
    ((_ : []), _) -> Left OldNetwork
    (_, _)        -> Left TooManyBlocks

fromGoodBlockchainCase :: Block -> Transaction
                       -> Map Addr Contract -> Map Addr Contract
                       -> Either BlockchainError Case
fromGoodBlockchainCase block tx preState postState =
  let toAddr   = txToAddr tx
      feeSchedule = EVM.FeeSchedule.metropolis
    in case (toAddr
           , Map.lookup toAddr preState
           , sender 1 tx
           , initTx tx block preState) of
      (0, _, _, _)       -> Left CreationUnsupported
      (_, Nothing, _, _) -> Left TargetMissing
      (_, _, Nothing, _) -> Left SignatureUnverified
      (_, _, _, Nothing) -> Left InvalidTx
      (_, Just c, Just origin, Just initState) -> Right $ Case
        (EVM.VMOpts
         { vmoptCode          = view code c
         , vmoptCalldata      = txData tx
         , vmoptValue         = txValue tx
         , vmoptAddress       = toAddr
         , vmoptCaller        = origin
         , vmoptOrigin        = origin
         , vmoptGas           = txGasLimit tx - fromIntegral (txGasCost feeSchedule tx)
         , vmoptGaslimit      = txGasLimit tx
         , vmoptNumber        = blockNumber block
         , vmoptTimestamp     = blockTimestamp block
         , vmoptCoinbase      = blockCoinbase block
         , vmoptDifficulty    = blockDifficulty block
         , vmoptBlockGaslimit = blockGasLimit block
         , vmoptGasprice      = txGasPrice tx
         , vmoptSchedule      = feeSchedule
         })
        initState
        (Just $ Expectation Nothing postState Nothing)

initTx :: Transaction -> Block -> Map Addr Contract -> Maybe (Map Addr Contract)
initTx tx block cs = do
  origin <- sender 1 tx
  let gasDeposit = fromIntegral (txGasPrice tx) * (txGasLimit tx)
      coinbase   = blockCoinbase block
  -- is there a neater lens?
  return $
    (Map.adjust ((nonce   %~ (+ 1))
               . (balance %~ (subtract gasDeposit))
               . (balance %~ (subtract $ txValue tx))) origin)
    . (Map.adjust (balance %~ (+ (txValue tx))) (txToAddr tx))
    . touchAccount origin
    . touchAccount (txToAddr tx)
    . touchAccount coinbase $ cs

touchAccount :: Addr -> Map Addr Contract -> Map Addr Contract
touchAccount a cs = let
  newAccount = Contract
    { _balance = 0
    , _code    = mempty
    , _nonce   = 0
    , _storage = mempty
    } in Map.insertWith (flip const) a newAccount cs

vmForCase :: Case -> EVM.VM
vmForCase x =
  EVM.makeVm (testVmOpts x)
    & EVM.env . EVM.contracts .~ realizeContracts (testContracts x)
    & EVM.execMode .~ EVM.ExecuteAsVMTest

interpret :: Stepper a -> EVM a
interpret =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> EVM a

    eval (Operational.Return x) =
      pure x

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          EVM.Exec.exec >>= interpret . k
        Stepper.Wait q ->
          do join (Fetch.zero q)
             interpret (k ())
        Stepper.Note _ ->
          interpret (k ())
        Stepper.Fail _ ->
          error "VMTest stepper not supposed to fail"
        Stepper.EVM m ->
          State.state (runState m) >>= interpret . k
