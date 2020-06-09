{-|

Nagios monitoring plugin for cgminer API

-}

module CheckCgminer where

import System.Nagios.Plugin ( runNagiosPlugin, addResult, CheckStatus (OK, Unknown, Warning, Critical), NagiosPlugin
                            , addPerfDatum, addPerfData, PerfValue (RealValue), UOM (NullUnit)
                            , ToPerfData, toPerfData, PerfValue (RealValue), PerfDatum (PerfDatum) )

import Options.Applicative
  ( execParser, info, header, progDesc, fullDesc, helper, Parser, option, long, short, metavar
  , value, help, str, auto, showDefault, infoOption, optional, flag)
import Network.Simple.TCP (connect, send, recv)
import Data.Aeson (encode)
import Data.Maybe (isNothing)
import Control.Monad (when)
import Control.Monad.Loops (unfoldWhileM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Ratio (approxRational)
import Paths_check_cgminer (version)
import Data.Version (showVersion)
import Data.Either (lefts)
import Numeric (showFFloat)
import Data.Time.Clock (nominalDay)
import Text.Printf (printf)
import Control.Exception (catches, IOException, SomeException, Handler (Handler))

import CgminerApi ( QueryApi (QueryApi), getStats, getSummary, decodeReply, Stats (Stats)
                  , TextRationalPairs, ReplyApi, includePowerConsumption, includeIdealPercentage
                  , PowerStrategy (DynamicPower, ConstantPower))
import Helper( getProfitability, Power (Watt), HashRates (Ghs), Bitcoins (Bitcoins), BitcoinUnit (Bitcoin)
             , Difficulty, EnergyRate (EnergyRate), EnergyUnit (KiloWattHour), MonetaryUnit (USD)
             , Price, cacheIO, getBitcoinDifficultyAndReward, WorkMode (WorkMode), getBitcoinPrice, Rate (Rate)
             , TimeUnit (Day, Second), getBitcoinAverageMiningFeeReward)

data CliOptions = CliOptions
  { host :: String
  , port :: String
  , temp_warning :: Double
  , temp_critical :: Double
  , hashrate_warning :: Double
  , hashrate_critical :: Double
  , hashrate_maximum :: Double
  , hashrate_below_ideal_warning :: Double
  , hashrate_below_ideal_critial :: Double
  , hash_unit :: String
  , fanspeed_low_warning :: Double
  , fanspeed_low_critical :: Double
  , fanspeed_high_warning :: Double
  , fanspeed_high_critical :: Double
  , voltage_high_warning :: Double
  , voltage_high_critical :: Double
  , frequency_high_warning :: Double
  , frequency_high_critical :: Double
  , power_strategy :: PowerStrategy
  , power_consumption :: Maybe Double
  , electricity_rate :: Double
  , profitability_warning :: Double
  , profitability_critical :: Double
  , block_fee_average_num :: Integer
  , block_reward :: Maybe Double
  , mining_fee_reward :: Maybe Double
  , pool_fee :: Double
  }

defaultTempWarningThreshold :: Double
defaultTempWarningThreshold = 90
defaultTempCriticalThreshold :: Double
defaultTempCriticalThreshold = 100
defaultHashRateWarningThreshold :: Double
defaultHashRateWarningThreshold = 4000
defaultHashRateCriticalThreshold :: Double
defaultHashRateCriticalThreshold = 3000
defaultHashRatePercentBelowIdealWarningThreshold :: Double
defaultHashRatePercentBelowIdealWarningThreshold = 0.95
defaultHashRatePercentBelowIdealCriticalThreshold :: Double
defaultHashRatePercentBelowIdealCriticalThreshold = 0.8
defaultHashUnit :: String
defaultHashUnit = "Ghs"
defaultFanSpeedLowWarningThreshold :: Double
defaultFanSpeedLowWarningThreshold = 999
defaultFanSpeedLowCriticalThreshold :: Double
defaultFanSpeedLowCriticalThreshold = 500
defaultFanSpeedHighWarningThreshold :: Double
defaultFanSpeedHighWarningThreshold = 9000
defaultFanSpeedHighCriticalThreshold :: Double
defaultFanSpeedHighCriticalThreshold = 10000
defaultVoltageHighWarningThreshold :: Double
defaultVoltageHighWarningThreshold = 20
defaultVoltageHighCriticalThreshold :: Double
defaultVoltageHighCriticalThreshold = 20
defaultFrequencyHighWarningThreshold :: Double
defaultFrequencyHighWarningThreshold = 5000
defaultFrequencyHighCriticalThreshold :: Double
defaultFrequencyHighCriticalThreshold = 5000
defaultElectricityRate :: Double
defaultElectricityRate = 0.1188 -- US national average 2019 USD/kWh
defaultProfitabilityWarningThreshold :: Double
defaultProfitabilityWarningThreshold = 0.25
defaultProfitabilityCriticalThreshold :: Double
defaultProfitabilityCriticalThreshold = 0
defaultPoolFee :: Double
defaultPoolFee = 0
defaultBlockFeeAverageNum :: Integer
defaultBlockFeeAverageNum = 10

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> option str
      ( long "host"
     <> short 'H'
     <> metavar "HOST"
     <> value "127.0.0.1"
     <> help "Hostname of cgminer API"
     <> showDefault
      )
  <*> option str
      ( long "port"
     <> short 'P'
     <> metavar "PORT"
     <> value "4028"
     <> help "Port of cgminer API"
     <> showDefault
      )
  <*> option auto
      ( long "temp_warn"
     <> short 't'
     <> metavar "NUMBER"
     <> value defaultTempWarningThreshold
     <> help "Warning temperature threshold in Celsius"
     <> showDefault
      )
  <*> option auto
      ( long "temp_crit"
     <> short 'T'
     <> metavar "NUMBER"
     <> value defaultTempCriticalThreshold
     <> help "Critical temperature threshold in Celsius"
     <> showDefault
      )
  <*> option auto
      ( long "hash_warn"
     <> short 'r'
     <> metavar "NUMBER"
     <> value defaultHashRateWarningThreshold
     <> help "Warning hash rate threshold"
     <> showDefault
      )
  <*> option auto
      ( long "hash_crit"
     <> short 'R'
     <> metavar "NUMBER"
     <> value defaultHashRateCriticalThreshold
     <> help "Critical hash rate threshold"
     <> showDefault
      )
  <*> option auto
      ( long "hash_maximum"
     <> metavar "NUMBER"
     <> value defaultMaximumHashRateThreshold
     <> help "Maximum Hashrate (Used with performance data)"
     <> showDefault
      )
  <*> option auto
      ( long "hash_percent_below_ideal_warn"
     <> short 'i'
     <> metavar "NUMBER"
     <> value defaultHashRatePercentBelowIdealWarningThreshold
     <> help "Warning hash percent below ideal hash rate threshold"
     <> showDefault
      )
  <*> option auto
      ( long "hash_percent_below_ideal_crit"
     <> short 'I'
     <> metavar "NUMBER"
     <> value defaultHashRatePercentBelowIdealCriticalThreshold
     <> help "Critical hash percent below ideal hash rate threshold"
     <> showDefault
      )
  <*> option str
      ( long "hashunit"
     <> metavar "STRING"
     <> value defaultHashUnit
     <> help "Hashing unit of measure"
     <> showDefault
      )
  <*> option auto
      ( long "fan_low_warn"
     <> short 'f'
     <> metavar "NUMBER"
     <> value defaultFanSpeedLowWarningThreshold
     <> help "Warning low fan speed threshold in RPMs"
     <> showDefault
      )
  <*> option auto
      ( long "fan_low_crit"
     <> short 'F'
     <> metavar "NUMBER"
     <> value defaultFanSpeedLowCriticalThreshold
     <> help "Critical low fan speed threshold in RPMs"
     <> showDefault
      )
   <*> option auto
      ( long "fan_high_warn"
     <> short 'n'
     <> metavar "NUMBER"
     <> value defaultFanSpeedHighWarningThreshold
     <> help "Warning high fan speed threshold in RPMs"
     <> showDefault
      )
   <*> option auto
      ( long "fan_high_crit"
     <> short 'N'
     <> metavar "NUMBER"
     <> value defaultFanSpeedHighCriticalThreshold
     <> help "Critical high fan speed threshold in RPMs"
     <> showDefault
      )
   <*> option auto
      ( long "volt_high_warn"
     <> metavar "NUMBER"
     <> value defaultVoltageHighWarningThreshold
     <> help "Warning high voltage threshold in Volts (Only supported for S9 miners)"
     <> showDefault
      )
   <*> option auto
      ( long "volt_high_crit"
     <> metavar "NUMBER"
     <> value defaultVoltageHighCriticalThreshold
     <> help "Critical high voltage threshold in Volts (Only supported for S9 miners)"
     <> showDefault
      )
   <*> option auto
      ( long "freq_high_warn"
     <> metavar "NUMBER"
     <> value defaultFrequencyHighWarningThreshold
     <> help "Warning high frequency threshold in Mhz (Only supported for S9 miners)"
     <> showDefault
      )
   <*> option auto
      ( long "freq_high_crit"
     <> metavar "NUMBER"
     <> value defaultFrequencyHighCriticalThreshold
     <> help "Critical high frequency threshold in Mhz (Only supported for S9 miners)"
     <> showDefault
      )
   <*> flag (ConstantPower Nothing) (DynamicPower Nothing)
      ( long "dynamic_power"
     <> short 'd'
     <> help "Enable dynamic power strategy calculation (Only supported for S17 miners, unless --device_power is also supplied.)"
      )
   <*> optional (option auto
      ( long "device_power"
     <> metavar "NUMBER"
     <> help "Override estimated device power consumption in Watt. When used with dynamic power flag, Watts per Gigahash is assumed."
      ))
   <*> option auto
      ( long "electric_rate"
     <> metavar "NUMBER"
     <> value defaultElectricityRate
     <> help "Default electricity rate in USD/kWh"
     <> showDefault
      )
   <*> option auto
      ( long "prof_warn"
     <> metavar "NUMBER"
     <> value defaultProfitabilityWarningThreshold
     <> help "Warning profitability threshold in USD/day"
     <> showDefault
      )
   <*> option auto
      ( long "prof_crit"
     <> metavar "NUMBER"
     <> value defaultProfitabilityCriticalThreshold
     <> help "Critical profitability threshold in USD/day"
     <> showDefault
      )
  <*> option auto
      ( long "fee_block_average_num"
     <> metavar "NUMBER"
     <> help "Number of past blocks to use for fee average"
     <> value defaultBlockFeeAverageNum
     <> showDefault
      )
  <*> optional (option auto
      ( long "block_reward"
     <> metavar "NUMBER"
     <> help "Override the block reward (default: API lookup)"
      ))
  <*> optional (option auto
      ( long "mining_fee_reward"
     <> metavar "NUMBER"
     <> help "Override the mining fee reward (default: API lookup)"
      ))
   <*> option auto
      ( long "pool_fee"
     <> metavar "NUMBER"
     <> value defaultPoolFee
     <> help "Pool fee percentage. Ex: 0.01 = 1%"
     <> showDefault
      )

anyTempsAreZero :: TextRationalPairs -> Bool
anyTempsAreZero = any ((== 0) . snd)
anyAboveThreshold :: TextRationalPairs -> Rational -> Bool
anyAboveThreshold t m = any ((>= m) . snd) t
anyBelowThreshold :: TextRationalPairs -> Rational -> Bool
anyBelowThreshold h m = any ((<= m) . snd) h

maximumTempThreshold :: Double
maximumTempThreshold = 120
minimumTempThreshold :: Double
minimumTempThreshold = 20
defaultMaximumHashRateThreshold :: Double
defaultMaximumHashRateThreshold = 10000
minimumHashRateThreshold :: Double
minimumHashRateThreshold = 0
maximumFanSpeedThreshold :: Double
maximumFanSpeedThreshold = 20000
minimumFanSpeedThreshold :: Double
minimumFanSpeedThreshold = 0
maximumVoltThreshold :: Double
maximumVoltThreshold = 20
minimumVoltThreshold :: Double
minimumVoltThreshold = 0
maximumFreqThreshold :: Double
maximumFreqThreshold = 5000
minimumFreqThreshold :: Double
minimumFreqThreshold = 0

data Thresholds = Thresholds TempThresholds HashThresholds HashIdealRatioThresholds
                  FanThresholds VoltageThresholds FrequencyThresholds ProfitabilityThresholds
data TempThresholds = TempThresholds HighWarning HighCritical
data FanThresholds = FanThresholds LowWarning LowCritical HighWarning HighCritical
data HashThresholds = HashThresholds LowWarning LowCritical Maximum
data HashIdealRatioThresholds = HashIdealRatioThresholds LowWarning LowCritical
data VoltageThresholds = VoltageThresholds HighWarning HighCritical
data FrequencyThresholds = FrequencyThresholds HighWarning HighCritical
data ProfitabilityThresholds = ProfitabilityThresholds LowWarning LowCritical
newtype HighWarning = HighWarning Rational
newtype HighCritical = HighCritical Rational
newtype LowWarning = LowWarning Rational
newtype LowCritical = LowCritical Rational
newtype Maximum = Maximum Double
data ProfitabilityFactors = ProfitabilityFactors EnergyRate Difficulty Price Bitcoins Bitcoins Rational

checkStats :: Stats
           -> Maybe ProfitabilityFactors
           -> Thresholds
           -> String -- | Hashing unit
           -> NagiosPlugin ()
checkStats (Stats _ mpc temps hashrates _ _ hashratesIdealRatioM
            fanspeeds voltages frequencies workMode)
           pfs
           (Thresholds
              (TempThresholds (HighWarning tw) (HighCritical tc))
              (HashThresholds (LowWarning hw) (LowCritical hc) (Maximum hmax))
              (HashIdealRatioThresholds (LowWarning hirw) (LowCritical hirc))
              (FanThresholds (LowWarning flw) (LowCritical flc)
               (HighWarning fhw) (HighCritical fhc))
              (VoltageThresholds (HighWarning vhw) (HighCritical vhc))
              (FrequencyThresholds (HighWarning freqhw) (HighCritical freqhc))
              profThresholds@(ProfitabilityThresholds (LowWarning profw) (LowCritical profc))
           ) hu = do


  when (anyTempsAreZero temps) $ addResult Warning "At least one temperature at 0 C"

  addResultIf anyAboveThreshold temps tc Critical "Temperature above critical threshold of " "C"
  addResultIf anyAboveThreshold temps tw Warning "Temperature above warning threshold of " "C"

  addResultIf anyBelowThreshold hashrates hc Critical "Hashrates below critical threshold of " $ T.pack hu
  addResultIf anyBelowThreshold hashrates hw Warning "Hashrates below warning threshold of " $ T.pack hu

  maybe (return ()) (\hashratesIdealRatio -> do
    addResultIf anyBelowThreshold [("", hashratesIdealRatio*100)] (hirc*100)
      Critical "Hashrate Ideal ratio below critcal threshold of " "%"
    addResultIf anyBelowThreshold [("", hashratesIdealRatio*100)] (hirw*100)
      Warning "Hashrate Ideal ratio below warning threshold of " "%"
    ) hashratesIdealRatioM

  addResultIf anyBelowThreshold fanspeeds flc Critical "Fan speed below critical threshold of " "RPM"
  addResultIf anyBelowThreshold fanspeeds flw Warning "Fan speed below warning threshold of " "RPM"

  addResultIf anyAboveThreshold fanspeeds fhc Critical "Fan speed above critical threshold of " "RPM"
  addResultIf anyAboveThreshold fanspeeds fhw Warning "Fan speed above warning threshold of " "RPM"

  addResultIf anyAboveThreshold voltages vhc Critical "Voltage above critical threshold of " "Volts"
  addResultIf anyAboveThreshold voltages vhw Warning "Voltage above warning threshold of " "Volts"

  addResultIf anyAboveThreshold frequencies freqhc Critical "Frequencies above critical threshold of " "Mhz"
  addResultIf anyAboveThreshold frequencies freqhw Warning "Frequencies above warning threshold of " "Mhz"

  -- https://bitcoin.stackexchange.com/questions/8568/equation-for-mining-profit
  -- TODO: Probably can use Maybe applicative to simplify this code.
  case pfs of
    Just (ProfitabilityFactors electricityRate difficulty price blockReward miningFeeReward poolFee) -> do
      let mProfitability = case mpc of
                            Just power -> Just $ getProfitability (Ghs (snd <$> hashrates)) difficulty
                                          blockReward miningFeeReward power electricityRate price poolFee
                            Nothing -> Nothing
      case mProfitability of
        Just p -> processProfitability p
        Nothing -> addResultOK Nothing
    -- Power factors couldn't be figured out so do nothing
    Nothing -> addResultOK Nothing

  -- Output performance data
  mapMPerfData addTempData temps
  mapMPerfData addHashData hashrates
  maybe (return ()) (\ir -> addPerfData' "IdealRatio" ir 0 1.5 hirw hirc) hashratesIdealRatioM
  mapMPerfData addFanData fanspeeds
  mapMPerfData addVoltData voltages
  mapMPerfData addFreqData frequencies
  maybe (return ()) (addPerfData . PerfDataWorkMode) workMode

  where
    maxTemp = maximum $ snd <$> temps
    minHashRates = minimum $ snd <$> hashrates
    addResultOK :: Maybe Rational -> NagiosPlugin ()
    addResultOK Nothing = addResult OK addResultOKStr
    addResultOK (Just prof) = addResult OK
      $ "Profitability is " <> ( T.pack . printf "%.4f" . toDouble) prof <> " USD/day, " <> addResultOKStr
    addResultOKStr = "Max temp: " <> (T.pack . show) (toDouble maxTemp) <> " C, "
      <> "Min hashrate: " <> T.pack (showFFloat Nothing (toDouble minHashRates) " " ++ hu ++ ", ")
      <> "Min fanspeed: " <> (T.pack . show) (toDouble $ minimum $ snd <$> fanspeeds) <> " RPM"

    processProfitability :: Rate -> NagiosPlugin ()
    processProfitability (Rate USD Second p) = processProfitability (Rate USD Day (p*24*60*60))
    processProfitability prof@(Rate USD Day p) = do
      let m t = "Profitability is below " <> (T.pack . show . toDouble) t <> " USD/day"
      addResultOK $ Just p
      when (p < profw) $ addResult Warning $ m profw
      when (p < profc) $ addResult Critical $ m profc
      addPerfData $ PerfDataProfitability prof profThresholds
    addTempData :: T.Text -> Rational -> NagiosPlugin ()
    addTempData s t = addPerfData' s t minimumTempThreshold maximumTempThreshold tw tc
    addHashData s t = addPerfData' s t minimumHashRateThreshold hmax hw hc
    addFanData s t = addPerfDatum s (RealValue $ fromRational t) NullUnit
                               (Just $ RealValue minimumFanSpeedThreshold)
                               (Just $ RealValue maximumFanSpeedThreshold) Nothing Nothing
    addVoltData s t = addPerfData' s t minimumVoltThreshold maximumVoltThreshold vhw vhc
    addFreqData s t = addPerfData' s t minimumFreqThreshold maximumFreqThreshold freqhw freqhc

    addPerfData' s t mint maxt w c = addPerfDatum s (RealValue $ fromRational t) NullUnit
                              (Just $ RealValue mint) (Just $ RealValue maxt)
                              (Just $ RealValue $ fromRational w) (Just $ RealValue $ fromRational c)
    mapMPerfData f = mapM_ (\x -> f ((removeSpaces . fst) x) (snd x))
    removeSpaces = T.map (\x -> if x == ' ' then '_' else x)

    toDouble :: Rational -> Double
    toDouble = fromRational
    addResultIf check values threshold resultType msg unit =
      when (check values threshold) $
      addResult resultType $ msg <> (T.pack . show) (toDouble threshold) <> " " <> unit

data PerfDataProfitability = PerfDataProfitability Rate ProfitabilityThresholds
instance ToPerfData PerfDataProfitability where
  toPerfData (PerfDataProfitability (Rate USD Second p) t) =
    toPerfData $ PerfDataProfitability (Rate USD Day (p * 24 * 60 * 60)) t
  toPerfData (PerfDataProfitability (Rate USD Day p) (ProfitabilityThresholds (LowWarning lw) (LowCritical lc))) =
    [ PerfDatum "profitability" (RealValue $ fromRational p) NullUnit Nothing Nothing
      ((Just . RealValue . fromRational) lw) ((Just . RealValue . fromRational) lc) ]
newtype PerfDataWorkMode = PerfDataWorkMode WorkMode
instance ToPerfData PerfDataWorkMode where
  toPerfData (PerfDataWorkMode (WorkMode i)) = [ PerfDatum "WorkMode" (RealValue $ fromIntegral i) NullUnit
                                                 (Just $ RealValue 0) (Just $ RealValue 2) Nothing Nothing
                                               ]

-- | Try to parse stats from miner. Return error `T.Text` if for failure.
tryCommand :: T.Text -> (ReplyApi -> Either String Stats) -> CliOptions -> IO (Either T.Text Stats)
tryCommand c f (CliOptions h p _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = do
  r <- catches (sendCGMinerCommand h p c)
       [ Handler (\(e :: IOException) ->
                    error $ "IOException while sending '" ++ T.unpack c ++ "' command: " ++ show e
                 )
       , Handler (\(e :: SomeException) -> error $ "sendCGMinerCommand error: " ++ show e)
       ]

  -- Uncomment for getting the raw reply from a miner for testing
  --print r
  --_ <- error ""

  if isNothing r
  then return $ Left $ "Could not parse reply from " <> (T.pack . show) h <> ":" <> (T.pack . show) p
  else do
    let Just r' = BL.fromStrict <$> r
    let mec = f <$> decodeReply r'

    case mec of
      Nothing -> return $ Left "Failed to decode reply from cgminer."
      Just ec ->
        case ec of
          Left s -> return $ Left $ T.pack s
          Right stats -> return $ Right stats


tryStats :: CliOptions -> IO (Either T.Text Stats)
tryStats = tryCommand "stats" getStats

trySummary :: CliOptions -> IO (Either T.Text Stats)
trySummary = tryCommand "summary" getSummary

execCheck :: CliOptions -> IO ()
execCheck opts@(CliOptions _ _ tw tc hw hc hmax hirw hirc hu flw flc fhw fhc vhw vhc freqhw
                freqhc ps mpc erd profw profc bfan mbr mmfr pfp) = do
  -- Try to get "stats" command first
  eStats <- tryStats opts
  processStats eStats

  -- Something went wrong with stats command so try "summary" next
  eSummary <- trySummary opts
  processStats eSummary

  runNagiosPlugin $ addResult Unknown $ T.concat $ lefts [ eStats, Left ", "
                                                         , eSummary
                                                         ]
  where
    processStats e =
      case e of
        Left _ -> return ()
        Right stats -> do
          pf <- getProfitabilityFactors
          let ps' = maybe ps (\d -> case ps of
                                 ConstantPower _ -> ConstantPower $ Just $ Watt $ toRational d
                                 DynamicPower _ -> DynamicPower $ Just $ Watt $ toRational d
                             ) mpc
          runNagiosPlugin $ checkStats ((includeIdealPercentage . includePowerConsumption ps') stats) pf thresholds hu

    getProfitabilityFactors :: IO (Maybe ProfitabilityFactors)
    getProfitabilityFactors = do
      dNr <- cacheIO "difficultyAndRewardCache" nominalDay getBitcoinDifficultyAndReward
      mr <- cacheIO "minerFeeRewardCache" nominalDay $ getBitcoinAverageMiningFeeReward bfan
      p <- cacheIO "priceCache" nominalDay getBitcoinPrice
      let er = EnergyRate USD KiloWattHour (toRational erd)
      return $ ProfitabilityFactors er <$> (fst <$> dNr) <*> p
        <*> maybe (snd <$> dNr) (\d -> Just (Bitcoins Bitcoin $ toRational d)) mbr
        <*> maybe mr (\d -> Just (Bitcoins Bitcoin $ toRational d)) mmfr <*> Just (toRational pfp)
    appRat v = approxRational v 0.0001
    thresholds = Thresholds (TempThresholds (HighWarning (appRat tw)) (HighCritical (appRat tc)))
                 (HashThresholds (LowWarning (appRat hw)) (LowCritical (appRat hc)) (Maximum hmax))
                 (HashIdealRatioThresholds (LowWarning (appRat hirw)) (LowCritical (appRat hirc)))
                 (FanThresholds (LowWarning (appRat flw)) (LowCritical (appRat flc))
                                (HighWarning (appRat fhw)) (HighCritical (appRat fhc)))
                 (VoltageThresholds (HighWarning $ appRat vhw) (HighCritical $ appRat vhc))
                 (FrequencyThresholds (HighWarning $ appRat freqhw) (HighCritical $ appRat freqhc))
                 (ProfitabilityThresholds (LowWarning $ appRat profw) (LowCritical $ appRat profc))

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execCheck
  where
    opts = info (helper <*> versionOption <*> cliOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based on cgminer API returned values"
     <> header "check_cgminer - Nagios monitoring plugin for cgminer API" )
    versionOption = infoOption ("check_cgminer " ++ showVersion version)
      ( long "version" <> short 'v' <> help "Show version information" )


sendCGMinerCommand :: String -> String -> T.Text -> IO (Maybe BS.ByteString)
sendCGMinerCommand h p s = connect h p $ \(connectionSocket, _) -> do
    send connectionSocket $ BL.toStrict . encode $ QueryApi s "0"
    mconcat <$> unfoldWhileM (Nothing /=) (recv connectionSocket 4096)
