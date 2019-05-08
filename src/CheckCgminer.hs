{-|

Nagios monitoring plugin for cgminer API

-}

module CheckCgminer where

import System.Nagios.Plugin ( runNagiosPlugin, addResult, CheckStatus (OK, Unknown, Warning, Critical), NagiosPlugin
                            , addPerfDatum, PerfValue (RealValue), UOM (NullUnit) )
import Options.Applicative
  ( execParser, info, header, progDesc, fullDesc, helper, Parser, option, long, short, metavar
  , value, help, str, auto, showDefault, infoOption)
import Network.Simple.TCP (connect, send, recv)
import Data.Aeson (encode)
import Control.Monad.Loops (unfoldWhileM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Ratio (approxRational)
import Paths_check_cgminer (version)
import Data.Version (showVersion)
import Data.Either (lefts)
import Numeric (showFFloat)

import CgminerApi ( QueryApi (QueryApi), getStats, getSummary, decodeReply, Stats (Stats), TextRationalPairs, ReplyApi)

data CliOptions = CliOptions
  { host :: String
  , port :: String
  , temp_warning :: Double
  , temp_error :: Double
  , hashrate_warning :: Double
  , hashrate_error :: Double
  , hash_unit :: String
  , fanspeed_low_warning :: Double
  , fanspeed_low_error :: Double
  , fanspeed_high_warning :: Double
  , fanspeed_high_error :: Double
  }

defaultTempWarningThreshold :: Double
defaultTempWarningThreshold = 90
defaultTempCriticalThreshold :: Double
defaultTempCriticalThreshold = 100
defaultHashRateWarningThreshold :: Double
defaultHashRateWarningThreshold = 4000
defaultHashRateCriticalThreshold :: Double
defaultHashRateCriticalThreshold = 3000
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
maximumHashRateThreshold :: Double
maximumHashRateThreshold = 10000
minimumHashRateThreshold :: Double
minimumHashRateThreshold = 0
maximumFanSpeedThreshold :: Double
maximumFanSpeedThreshold = 20000
minimumFanSpeedThreshold :: Double
minimumFanSpeedThreshold = 0

data Thresholds = Thresholds Rational Rational Rational Rational Rational Rational Rational Rational

checkStats :: Stats -> Thresholds
           -> String -- | Hashing unit
           -> NagiosPlugin ()
checkStats (Stats temps hashrates fanspeeds) (Thresholds tw tc hw hc flw flc fhw fhc) hu = do
  let maxTemp = maximum $ snd <$> temps
  let minHashRates = minimum $ snd <$> hashrates
  addResult OK
     $ "Max temp: " <> (T.pack . show) (toDouble maxTemp) <> " C, "
    <> "Min hashrate: " <> T.pack (showFFloat Nothing (toDouble minHashRates) " " ++ hu ++ ", ")
    <> "Min fanspeed: " <> (T.pack . show) (toDouble $ minimum $ snd <$> fanspeeds) <> " RPM"

  if anyTempsAreZero temps
  then addResult Warning "At least one temperature at 0 C"
  else return ()

  addResultIf anyAboveThreshold temps tc Critical "Temperature above critical threshold of " "C"
  addResultIf anyAboveThreshold temps tw Warning "Temperature above warning threshold of " "C"

  addResultIf anyBelowThreshold hashrates hc Critical "Hashrates below critical threshold of " $ T.pack hu
  addResultIf anyBelowThreshold hashrates hw Warning "Hashrates below warning threshold of " $ T.pack hu

  addResultIf anyBelowThreshold fanspeeds flc Critical "Fan speed below critical threshold of " "RPM"
  addResultIf anyBelowThreshold fanspeeds flw Warning "Fan speed below warning threshold of " "RPM"

  addResultIf anyAboveThreshold fanspeeds fhc Critical "Fan speed above critical threshold of " "RPM"
  addResultIf anyAboveThreshold fanspeeds fhw Warning "Fan speed above warning threshold of " "RPM"

  -- Go through each measurement and it to performance data output
  mapMPerfData addTempData temps
  mapMPerfData addHashData hashrates
  mapMPerfData addFanData fanspeeds

  where
    addTempData :: T.Text -> Rational -> NagiosPlugin ()
    addTempData s t = addPerfData s t minimumTempThreshold maximumTempThreshold tw tc
    addHashData s t = addPerfData s t minimumHashRateThreshold maximumHashRateThreshold hw hc
    addFanData s t = addPerfDatum s (RealValue $ fromRational t) NullUnit
                               (Just $ RealValue minimumFanSpeedThreshold)
                               (Just $ RealValue maximumFanSpeedThreshold) Nothing Nothing

    addPerfData s t mint maxt w c = addPerfDatum s (RealValue $ fromRational t) NullUnit
                              (Just $ RealValue mint) (Just $ RealValue maxt)
                              (Just $ RealValue $ fromRational w) (Just $ RealValue $ fromRational c)
    mapMPerfData f l = mapM_ (\x -> f ((removeSpaces . fst) x) (snd x)) l
    removeSpaces = T.map (\x -> if x == ' ' then '_' else x)

    toDouble :: Rational -> Double
    toDouble = fromRational
    addResultIf check values threshold resultType msg unit =
      if check values threshold
      then addResult resultType $ msg <> (T.pack . show) (toDouble threshold) <> " " <> unit
      else return ()


-- | Try to parse stats from miner. Return error `T.Text` if for failure.
tryCommand :: T.Text -> (ReplyApi -> Either String Stats) -> CliOptions -> IO (Either T.Text Stats)
tryCommand c f (CliOptions h p _ _ _ _ _ _ _ _ _) = do
  r <- sendCGMinerCommand h p c

  -- Uncomment for getting the raw reply from a miner for testing
  --print r
  --_ <- error ""

  if r == Nothing
  then return $ Left $ "Could not parse reply from " <> ((T.pack . show) h) <> ":" <> ((T.pack . show) p)
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
execCheck opts@(CliOptions _ _ tw tc hw hc hu flw flc fhw fhc) = do

  -- Try to get "stats" command first
  eStats <- tryStats opts

  case eStats of
    Left _ -> return ()
    Right stats -> runNagiosPlugin $ checkStats stats (Thresholds (appRat tw) (appRat tc)
                                                    (appRat hw) (appRat hc)
                                                    (appRat flw) (appRat flc)
                                                    (appRat fhw) (appRat fhc)) hu

  -- Something went wrong with stats command so try "summary" next
  eSummary <- trySummary opts

  case eSummary of
    Left _ -> return ()
    Right stats -> runNagiosPlugin $ checkStats stats (Thresholds (appRat tw) (appRat tc)
                                                    (appRat hw) (appRat hc)
                                                    (appRat flw) (appRat flc)
                                                    (appRat fhw) (appRat fhc)) hu

  runNagiosPlugin $ addResult Unknown $ T.concat $ lefts [ eStats, Left ", "
                                                         , eSummary
                                                         ]
  where
    appRat v = approxRational v 0.0001

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
    mconcat <$> unfoldWhileM ((/=) Nothing) (recv connectionSocket 4096)
