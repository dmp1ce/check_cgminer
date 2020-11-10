{-|

Various Bitcoin specific helper functions

-}

module Helper where

import qualified Network.Wreq as W
import Control.Lens ((^?), (^..))
import Data.Aeson.Lens (key, _Number, _String, _Integer, values)
import Data.Text.Read (rational)
import Text.Read (readEither)
import qualified Data.Serialize as S
import qualified System.Directory as D
import System.FilePath ((</>))
import qualified Data.ByteString as B
import qualified Data.Time.Clock as C
import GHC.Generics (Generic)
import Data.String.Conversions (cs)
import Control.Concurrent (threadDelay)

-- Units
data BitcoinUnit = Bitcoin deriving (Show, Generic)
instance S.Serialize BitcoinUnit
data MonetaryUnit = USD deriving (Show, Generic)
instance S.Serialize MonetaryUnit
data EnergyUnit = KiloWattHour
data TimeUnit = Second | Day deriving (Eq, Show)

-- Profitability variables
newtype HashRates = Ghs [Rational]
newtype Difficulty = Difficulty Rational deriving (Show, S.Serialize)
data Bitcoins = Bitcoins BitcoinUnit Rational deriving (Show, Generic)
instance S.Serialize Bitcoins
newtype Power = Watt Rational deriving (Eq, Show)
data EnergyRate = EnergyRate MonetaryUnit EnergyUnit Rational
data Price = Price MonetaryUnit Rational deriving (Show, Generic)
instance S.Serialize Price
data Time = Time TimeUnit Rational deriving (Eq, Show)
data Rate = Rate MonetaryUnit TimeUnit Rational deriving (Show)

-- | Blockchain.info RawBlock data
data RawBlock = RawBlock { _blockHash :: B.ByteString    -- ^ Block hash
                         , _previousBlock :: B.ByteString -- ^ Previous block hash
                         , _fee :: Rational        -- ^ Bitcoin fee
                         , _height :: Integer      -- ^ Block height
                         }
  deriving (Show, Generic)
instance S.Serialize RawBlock

-- | Get current device profitability for a devices current hash rates (USD / second)
--   https://en.bitcoin.it/wiki/Difficulty
getProfitability :: HashRates -- ^ All hash rates on device (hash / second)
                 -> Difficulty -- ^ Current bitcoin difficulty (hash)
                 -> Bitcoins -- ^ Current block reward (BTC)
                 -> Bitcoins -- ^ Estimated fees from block (BTC)
                 -> Power -- ^ Devices power consumption per hash (watts)
                 -> EnergyRate -- ^ Current electricity rate (USD / watt * hour)
                 -> Price -- ^ Current exchange rate (USD / BTC)
                 -> Rational -- ^ Pool fee percentage
                 -> Rate -- ^ USD/day
getProfitability hr d br fr (Watt pc)
                 (EnergyRate USD KiloWattHour er) bp pf =
  let
    Rate USD Second revenueRateUSD = revenueRate hr d br fr bp

    -- Convert Kilowatt Hours to watt seconds
    expenseRatePerSecond = ((pc / 1000) * er) / (60 * 60) -- In USD/second
    poolFee = revenueRateUSD * pf
    profitRatePerDay = ((revenueRateUSD - expenseRatePerSecond - poolFee) * (24 * 60 * 60))

  in Rate USD Day profitRatePerDay

-- | https://en.bitcoin.it/wiki/Difficulty#How_soon_might_I_expect_to_generate_a_block.3F
calculateTimeToGenerateBlock :: HashRates -> Difficulty -> Maybe Time
calculateTimeToGenerateBlock (Ghs hr) (Difficulty d)
  | sum hr == 0 = Nothing
  | otherwise = Just $ Time Second $ (d * (2^ (32::Integer))) / (sum hr * 10^(9::Integer))

revenueRate :: HashRates -- ^ All hash rates on device
            -> Difficulty -- ^ Current difficulty
            -> Bitcoins -- ^ Current block reward
            -> Bitcoins -- ^ Estimated fees collected from block
            -> Price -- ^ Current bitcoin price
            -> Rate
revenueRate hr d (Bitcoins Bitcoin br) (Bitcoins Bitcoin fr) (Price USD p) =
  case calculateTimeToGenerateBlock hr d of
    Nothing -> Rate USD Second 0
    Just (Time Second timeToGenerateBlock) -> Rate USD Second $ calc' timeToGenerateBlock
    Just (Time Day timeToGenerateBlock) -> Rate USD Second $ calc' $ timeToGenerateBlock / (60*60*24)
  where
    calc' t = ( (br + fr) / t ) * p

data MiningDevice = AntminerS9SE | AntminerS9k | AntminerDR5 | AntminerS9 | AntminerS17
                  | AntminerS15 | AntminerS17Pro | AntminerZ9Mini | Whatminer
                  | AntminerS17Vnish
  deriving (Ord, Eq, Show)

-- | Default power consumption lookup
--   TODO: Fill in remaining devices
newtype WorkMode = WorkMode Int deriving (Eq, Show)
miningDevicePowerConsumption :: MiningDevice -> Maybe WorkMode -> Either String Power
miningDevicePowerConsumption  = l
  where
    l AntminerS9SE Nothing = Right $ Watt 1280
    l AntminerS9k Nothing = Left "missing Antminer S9k"
    l AntminerDR5 Nothing = Left "missing Antiner DR5"
    l AntminerS9 Nothing = Right $ Watt 1323
    l AntminerS17Vnish Nothing = Right $ Watt 2800
    l AntminerS17 (Just (WorkMode 1)) = Right $ Watt 1800
    l AntminerS17 (Just (WorkMode 2)) = Right $ Watt 2385
    l AntminerS17 Nothing = l AntminerS17 (Just (WorkMode 2))
    l AntminerS15 Nothing = Right $ Watt 1596
    l AntminerS17Pro (Just (WorkMode 1)) = Right $ Watt 2094
    l AntminerS17Pro (Just (WorkMode 2)) = Right $ Watt 2500
    l AntminerS17Pro Nothing = l AntminerS17Pro (Just (WorkMode 1))
    l AntminerZ9Mini Nothing = Left "missing Antminer Z9Mini"
    l Whatminer Nothing = Right $ Watt 2145
    l d m = Left $ "Fill in remaining lookup table" ++ "type: " ++ show d ++ ", mode: " ++ show m

-- Price
getBitcoinPrice :: IO (Maybe Price)
getBitcoinPrice = do
  -- https://apiv2.bitcoinaverage.com/#ticker-data-per-symbol
  r <- W.get "https://apiv2.bitcoinaverage.com/indices/global/ticker/BTCUSD"
  case toRational <$> r ^? W.responseBody . key "averages" . key "day" . _Number of
    Just r' -> return $ Just (Price USD r')
    _ -> return Nothing

-- Difficulty
getBitcoinDifficulty :: IO (Maybe Difficulty)
getBitcoinDifficulty = do
  r <- W.get "https://blockchain.info/q/getdifficulty"
  d <- case rational . cs <$> r ^? W.responseBody of
         Just (Right (r',_)) -> return $ Just $ Difficulty r'
         _ -> return Nothing
  return d

-- Reward
getBitcoinReward :: IO (Maybe Bitcoins)
getBitcoinReward = do
  r <- W.get "https://blockchain.info/q/bcperblock"
  d <- case rational . cs <$> r ^? W.responseBody of
         Just (Right (r',_)) -> return $ Just $ Bitcoins Bitcoin r'
         _ -> return Nothing
  return d

-- Difficulty and Block Reward
getBitcoinDifficultyAndReward :: IO (Maybe (Difficulty, Bitcoins))
getBitcoinDifficultyAndReward = do
  d <- getBitcoinDifficulty
  r <- getBitcoinReward
  return $ (,) <$> d <*> r

 -- Archived and original Difficulty and Block Reward API call
getBitcoinDifficultyAndRewardFromBitcoinChain :: IO (Maybe (Difficulty, Bitcoins))
getBitcoinDifficultyAndRewardFromBitcoinChain = do
  r <- W.get "https://api-r.bitcoinchain.com/v1/status"
  d <- case rational <$> r ^? W.responseBody . key "difficulty" . _String of
         Just (Right (r',_)) -> return $ Just $ Difficulty r'
         _ -> return Nothing
  reward <- case rational <$> r ^? W.responseBody . key "reward" . _String of
              Just (Right (r',_)) -> return $ Just $ Bitcoins Bitcoin r'
              _ -> return Nothing
  return $ (,) <$> d <*> reward

-- | Average mining fee per block
getBitcoinAverageMiningFeeReward :: Integer -> IO (Maybe Bitcoins)
getBitcoinAverageMiningFeeReward = getBitcoinAverageBlockFeesBlockchainInfo

-- | Get the average fee for past blocks
--   Can take up to about 1 second wait per block.
getBitcoinAverageBlockFeesBlockchainInfo :: Integer -- ^ How many blocks back to average
                                         -> IO (Maybe Bitcoins)
getBitcoinAverageBlockFeesBlockchainInfo h
  | h <= 0 = return $ Just $ Bitcoins Bitcoin 0
  | otherwise = do
  l <- getLatestBitcoinBlockCached
  t <- f h l
  return $ Just $ Bitcoins Bitcoin $ (t / toRational h) / 100000000
  where
    f :: Integer -> Maybe RawBlock -> IO Rational
    f 0 _ = return 0
    f h' b =
      case b of
        Just b' -> do
          p <- getBitcoinRawBlockCached $ _previousBlock b'
          pf <- f (h' -1) p
          return $  _fee b' + pf
        Nothing -> return 0

-- Archived and original Fee average from blockchain.com
getBitcoinAverageBlockFeesBitcoinchain :: IO (Maybe Bitcoins)
getBitcoinAverageBlockFeesBitcoinchain = do
  r <- W.get "https://api-r.bitcoinchain.com/v1/blocks/100"
  let rats = rational <$> r ^.. W.responseBody . values . key "fee" . _String
  case (/ (toRational . length) rats) <$> (sum . (fst <$>) <$> sequenceA rats) of
    Right r' -> return $ Just $ Bitcoins Bitcoin r'
    Left _ -> return Nothing

-- Get the current height of the bitcoin blockchain
getBitcoinBlockHeight :: IO (Maybe Integer)
getBitcoinBlockHeight = do
  r <- W.get "https://blockchain.info/latestblock"
  return $ r ^? W.responseBody . key "height" . _Integer

-- Get the latest block
getLatestBitcoinBlock :: IO (Maybe RawBlock)
getLatestBitcoinBlock = do
  r <- W.get "https://blockchain.info/latestblock"
  case r ^? W.responseBody . key "hash" . _String of
    Nothing -> return Nothing
    Just h ->  getBitcoinRawBlock (cs h)

-- | Get latest block with cache
getLatestBitcoinBlockCached :: IO (Maybe RawBlock)
getLatestBitcoinBlockCached = delayedCacheIO "latestblock" 300 15 getLatestBitcoinBlock

-- | Get block by hash
getBitcoinRawBlock :: B.ByteString -> IO (Maybe RawBlock)
getBitcoinRawBlock h = do
  r <- W.get $ "https://blockchain.info/rawblock/" <> cs h
  return $ RawBlock <$> (cs <$> r ^? W.responseBody . key "hash" . _String)
                    <*> (cs <$> r ^? W.responseBody . key "prev_block" . _String)
                    <*> (toRational <$> r ^? W.responseBody . key "fee" . _Number)
                    <*> (r ^? W.responseBody . key "height" . _Integer)

-- | Get block by hash with cache
getBitcoinRawBlockCached :: B.ByteString -> IO (Maybe RawBlock)
getBitcoinRawBlockCached h = delayedCacheIO ("block_" <> cs h) (C.nominalDay * 10000) 15 $ getBitcoinRawBlock h


-- Caching
newtype CacheUTCTime = CacheUTCTime C.UTCTime deriving (Eq, Show)
instance S.Serialize CacheUTCTime where
  put (CacheUTCTime t)= S.put $ show t
  get = do
    l <- S.getListOf S.get
    case readEither l of
      Right t -> return $ CacheUTCTime t
      Left m -> fail m

data CacheData a = CacheData CacheUTCTime a deriving (Generic, Show)
instance (S.Serialize a) => S.Serialize (CacheData a)

-- | Cache an IO call if it is 'Serialize'
cacheIO :: (S.Serialize s)
        => FilePath -- ^ Key for storing the and retrieving the cache
        -> C.NominalDiffTime -- ^ Amount of time to invalidate previous cache
        -> IO s     -- ^ IO function to cache
        -> IO s
cacheIO k i f = do
  keyPath <- initCache k
  cacheExists <- validCache keyPath i
  case cacheExists of
    Nothing -> cacheIO' keyPath f
    Just r -> return r
  where
    cacheIO' k' f' = do r <- f'
                        utct <- C.getCurrentTime
                        B.writeFile k' (S.encode $ CacheData (CacheUTCTime utct) r)
                        return r

-- | Only allow one process access to IO at a time to avoid overloading the system when cache expires.
delayedCacheIO :: (S.Serialize s)
        => FilePath -- ^ Key for storing the and retrieving the cache
        -> C.NominalDiffTime -- ^ Amount of time to invalidate previous cache
        -> C.NominalDiffTime -- ^ Amount of time to delay if cache is being rebuilt
        -> IO s     -- ^ IO function to cache
        -> IO s
delayedCacheIO k i d f = do
  keyPath <- initCache k
  cacheExists <- validCache keyPath i
  case cacheExists of
    Nothing -> delayLoop d
    Just r -> return r
  where
    delayLoop delay = do
      let lockName = "." <> k <> ".lock"
      lockPath <- initCache lockName
      lockCache <- (validCache lockName delay :: IO (Maybe Bool))
      case lockCache of
        Nothing -> do
          -- putStrLn "Lock missing. Building cache."
          -- create lock, run cacheIO, delete lock
          utct <- C.getCurrentTime
          B.writeFile lockPath (S.encode $ CacheData (CacheUTCTime utct) True)
          r <- cacheIO k i f
          deleteCache lockName
          return r
        Just _ -> do
          -- putStrLn "Lock found. Delay for 1 second"
          threadDelay 1000000
          -- putStrLn "Starting delay loop again"
          delayLoop delay

-- | Check to see if a cache is valid. Returns result if valid.
validCache :: (S.Serialize s) => FilePath -> C.NominalDiffTime -> IO (Maybe s)
validCache k i = do
  keyPath <- initCache k

  cacheExists <- D.doesFileExist keyPath
  if cacheExists
    then do keyFileContents <- B.readFile keyPath
            case S.decode keyFileContents of
              Right (CacheData (CacheUTCTime d') r) ->
                do t <- C.getCurrentTime
                   if C.diffUTCTime t d' >= i
                     then return Nothing
                     else return $ Just r
              Left _ -> return Nothing
    else return Nothing

deleteCache :: FilePath -> IO ()
deleteCache k = do
  dk <- initCache k
  D.removeFile dk

-- | Initialize cache and return path to where cache exists
initCache :: FilePath -> IO FilePath
initCache k = do
  d <- D.getXdgDirectory D.XdgCache "check_cgminer"
  D.createDirectoryIfMissing True d
  return $ d </> k

-- For debugging
timeToDouble :: Time -> Double
timeToDouble (Time Second t) = fromRational t
timeToDouble (Time Day t) = fromRational (t * 24 * 60 * 60)
