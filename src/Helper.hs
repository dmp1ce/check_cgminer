{-|

Various Bitcoin specific helper functions

-}

module Helper where

import qualified Network.Wreq as W
import Control.Lens ((^?), (^..))
import Data.Aeson.Lens (key, _Number, _String, values)
import Data.Text.Read (rational)
import Text.Read (readEither)
import qualified Data.Serialize as S
import qualified System.Directory as D
import System.FilePath ((</>))
import qualified Data.ByteString as B
import qualified Data.Time.Clock as C
import GHC.Generics (Generic)

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

-- | Get current device profitability for a devices current hash rates (USD / second)
--   https://en.bitcoin.it/wiki/Difficulty
getProfitability :: HashRates -- | All hash rates on device (hash / second)
                 -> Difficulty -- | Current bitcoin difficulty (hash)
                 -> Bitcoins -- | Current block reward (BTC)
                 -> Bitcoins -- | Estimated fees from block (BTC)
                 -> Power -- | Devices power consumption (watts)
                 -> EnergyRate -- | Current electricity rate (USD / watt * hour)
                 -> Price -- | Current exchange rate (USD / BTC)
                 -> Rational -- | Pool fee percentage
                 -> Rate -- | USD/day
getProfitability hr d br fr (Watt pc)
                 (EnergyRate USD KiloWattHour er) bp pf =
  let
    Rate USD Second revenueRateUSD = revenueRate hr d br fr bp

    -- Convert Kilowatt Hours to watt seconds
    expenseRatePerSecond = ((pc / 1000) * er) / (60 * 60) -- In USD/second
    poolFee = revenueRateUSD * pf
    profitRatePerDay = ((revenueRateUSD - expenseRatePerSecond - poolFee) * (24 * 60 * 60))

  in Rate USD Day $ profitRatePerDay

-- | https://en.bitcoin.it/wiki/Difficulty#How_soon_might_I_expect_to_generate_a_block.3F
calculateTimeToGenerateBlock :: HashRates -> Difficulty -> Maybe Time
calculateTimeToGenerateBlock (Ghs hr) (Difficulty d)
  | sum hr == 0 = Nothing
  | otherwise = Just $ Time Second $ (d * (2^ (32::Integer))) / (sum hr * 10^(9::Integer))

revenueRate :: HashRates -- | ALl hash rates on device
            -> Difficulty -- | Current difficulty
            -> Bitcoins -- | Current block reward
            -> Bitcoins -- | Estimated fees collected from block
            -> Price -- | Current bitcoin price
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

-- Difficulty and Block Reward
getBitcoinDifficultyAndReward :: IO (Maybe (Difficulty, Bitcoins))
getBitcoinDifficultyAndReward = do
  r <- W.get "https://api-r.bitcoinchain.com/v1/status"
  d <- case rational <$> r ^? W.responseBody . key "difficulty" . _String of
         Just (Right (r',_)) -> return $ Just $ Difficulty r'
         _ -> return Nothing
  reward <- case rational <$> r ^? W.responseBody . key "reward" . _String of
              Just (Right (r',_)) -> return $ Just $ Bitcoins Bitcoin r'
              _ -> return Nothing
  return $ (,) <$> d <*> reward

-- Average mining fee per block
getBitcoinAverageMiningFeeReward :: IO (Maybe Bitcoins)
getBitcoinAverageMiningFeeReward = do
  r <- W.get "https://api-r.bitcoinchain.com/v1/blocks/100"
  let rats = rational <$> r ^.. W.responseBody . values . key "fee" . _String
  case (/ (toRational . length) rats) <$> (sum . (fst <$>) <$> sequenceA rats) of
    Right r' -> return $ Just $ Bitcoins Bitcoin r'
    Left _ -> return Nothing

-- Block reward
getBitcoinBlockReward :: IO (Maybe Bitcoins)
getBitcoinBlockReward = do
  r <- W.get "https://api-r.bitcoinchain.com/v1/blocks/100"
  let rats = rational <$> r ^.. W.responseBody . values . key "fee" . _String
  case (/ (toRational . length) rats) <$> (sum . (fst <$>) <$> sequenceA rats) of
    Right r' -> return $ Just $ Bitcoins Bitcoin r'
    Left _ -> return Nothing

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
        => FilePath -- | Key for storing the and retrieving the cache
        -> C.NominalDiffTime -- | Amount of time to invalidate previous cache
        -> IO s     -- | IO function to cache
        -> IO s
cacheIO k i f = do
  d <- D.getXdgDirectory D.XdgCache "check_cgminer"
  D.createDirectoryIfMissing True d
  let keyPath = d </> k

  cacheExists <- D.doesFileExist keyPath
  if cacheExists
    then do keyFileContents <- B.readFile keyPath
            case S.decode keyFileContents of
              Right (CacheData (CacheUTCTime d') r) ->
                do t <- C.getCurrentTime
                   if C.diffUTCTime t d' >= i
                     then cacheIO' keyPath f
                     else return r
              Left _ -> cacheIO' keyPath f
    else cacheIO' keyPath f
  where
    cacheIO' k' f' = do r <- f'
                        utct <- C.getCurrentTime
                        B.writeFile k' (S.encode $ CacheData (CacheUTCTime utct) r)
                        return r

deleteCache :: FilePath -> IO ()
deleteCache k = do
  d <- D.getXdgDirectory D.XdgCache "check_cgminer"
  D.createDirectoryIfMissing True d
  D.removeFile $ d </> k


-- For debugging
timeToDouble :: Time -> Double
timeToDouble (Time Second t) = fromRational t
timeToDouble (Time Day t) = fromRational (t * 24 * 60 * 60)
