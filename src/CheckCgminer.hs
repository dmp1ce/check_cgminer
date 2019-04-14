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
import Data.ByteString.Lazy (toStrict, fromStrict)
import Control.Monad.Loops (unfoldWhileM)
import qualified Data.Text as T
import Data.Ratio (approxRational)
import Paths_check_cgminer (version)
import Data.Version (showVersion)

import CgminerApi ( QueryApi (QueryApi), getStats, decodeReply, Stats (Stats), TextRationalPairs)

data CliOptions = CliOptions
  { host :: String
  , port :: String
  , temp_warning :: Double
  , temp_error :: Double
  , hashrate_warning :: Double
  , hashrate_error :: Double
  , fanspeed_warning :: Double
  , fanspeed_error :: Double
  }

defaultTempWarningThreshold :: Double
defaultTempWarningThreshold = 90
defaultTempCriticalThreshold :: Double
defaultTempCriticalThreshold = 100
defaultHashRateWarningThreshold :: Double
defaultHashRateWarningThreshold = 4000
defaultHashRateCriticalThreshold :: Double
defaultHashRateCriticalThreshold = 3000
defaultFanSpeedWarningThreshold :: Double
defaultFanSpeedWarningThreshold = 4000
defaultFanSpeedCriticalThreshold :: Double
defaultFanSpeedCriticalThreshold = 3000

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
     <> help "Warning hash rate threshold in Gh/s"
     <> showDefault
      )
  <*> option auto
      ( long "hash_crit"
     <> short 'R'
     <> metavar "NUMBER"
     <> value defaultHashRateCriticalThreshold
     <> help "Critical hash rate threshold in Gh/s"
     <> showDefault
      )
  <*> option auto
      ( long "fan_warn"
     <> short 'f'
     <> metavar "NUMBER"
     <> value defaultFanSpeedWarningThreshold
     <> help "Warning fan speed threshold in RPMs"
     <> showDefault
      )
  <*> option auto
      ( long "han_crit"
     <> short 'F'
     <> metavar "NUMBER"
     <> value defaultFanSpeedCriticalThreshold
     <> help "Critical fan speed threshold in RPMs"
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
maximumFanSpeedThreshold = 10000
minimumFanSpeedThreshold :: Double
minimumFanSpeedThreshold = 0

data Thresholds = Thresholds Rational Rational Rational Rational Rational Rational

checkStats :: Stats -> Thresholds -> NagiosPlugin ()
checkStats (Stats temps hashrates fanspeeds) (Thresholds tw tc hw hc fw fc) = do
  let maxTemp = maximum $ snd <$> temps
  let minHashRates = minimum $ snd <$> hashrates
  addResult OK
     $ "Max temp: " <> (T.pack . show) (toDouble maxTemp) <> " C, "
    <> "Min hashrate: " <> (T.pack . show) (toDouble minHashRates) <> " Ghs, "
    <> "Min fanspeed: " <> (T.pack . show) (toDouble $ minimum $ snd <$> fanspeeds) <> " RPM"

  if anyTempsAreZero temps
  then addResult Warning "At least one temperature at 0 C"
  else return ()

  if anyAboveThreshold temps tc
  then addResult Critical $ "Temperature exceeds critical threshold of " <> (T.pack . show) (toDouble tc) <> " C"
  else return ()

  if anyAboveThreshold temps tw
  then addResult Warning $ "Temperature exceeds warning threshold of " <> (T.pack . show) (toDouble tw) <> " C"
  else return ()

  if anyBelowThreshold hashrates hc
  then addResult Critical $ "Hashrate exceeds critical threshold of " <> (T.pack . show) (toDouble hc) <> " Ghs"
  else return ()

  if anyBelowThreshold hashrates hw
  then addResult Warning $ "Hashrate exceeds warning threshold of " <> (T.pack . show) (toDouble hw) <> " Ghs"
  else return ()

  if anyBelowThreshold fanspeeds fc
  then addResult Critical $ "Fan speed exceeds critical threshold of " <> (T.pack . show) (toDouble fc) <> " RPM"
  else return ()

  if anyBelowThreshold fanspeeds fw
  then addResult Warning $ "Fan speed exceeds warning threshold of " <> (T.pack . show) (toDouble fw) <> " RPM"
  else return ()

  -- Go through each measurement and it to performance data output
  mapMPerfData addTempData temps
  mapMPerfData addHashData hashrates
  mapMPerfData addFanData fanspeeds

  where
    addTempData :: T.Text -> Rational -> NagiosPlugin ()
    addTempData s t = addPerfData s t minimumTempThreshold maximumTempThreshold tw tc
    addHashData s t = addPerfData s t minimumHashRateThreshold maximumHashRateThreshold hw hc
    addFanData s t = addPerfData s t minimumFanSpeedThreshold maximumFanSpeedThreshold fw fc

    addPerfData s t mint maxt w c = addPerfDatum s (RealValue $ fromRational t) NullUnit
                              (Just $ RealValue mint) (Just $ RealValue maxt)
                              (Just $ RealValue $ fromRational w) (Just $ RealValue $ fromRational c)
    mapMPerfData f l = mapM_ (\x -> f (fst x) (snd x)) l

    toDouble :: Rational -> Double
    toDouble = fromRational

execCheck :: CliOptions -> IO ()
execCheck (CliOptions h p tw tc hw hc fw fc) = do
  r <- connect h p $ \(connectionSocket, _) -> do
    send connectionSocket $ toStrict . encode $ QueryApi "stats" "0"
    mconcat <$> unfoldWhileM ((/=) Nothing) (recv connectionSocket 4096)

  if r == Nothing
  then runNagiosPlugin $ addResult Unknown ( "Could not parse reply from "
                                          <> ((T.pack . show) h) <> ":" <> ((T.pack . show) p))
  else do
    let Just r' = fromStrict <$> r
    let Just (Right stats) = getStats <$> decodeReply r'

    runNagiosPlugin $ checkStats stats $ Thresholds (appRat tw) (appRat tc)
                                                    (appRat hw) (appRat hc)
                                                    (appRat fw) (appRat fc)
  where
    appRat v = approxRational v 0.0001

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execCheck
  where
    opts = info (helper <*> versionOption <*> cliOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based cgminer API returned values"
     <> header "check_cgminer - Nagios monitoring plugin for cgminer API" )
    versionOption = infoOption ("check_cgminer " ++ showVersion version)
      ( long "version" <> short 'v' <> help "Show version information" )
