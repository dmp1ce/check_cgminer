{-|

Nagios monitoring plugin for cgminer API

-}

module CheckCgminer where

import System.Nagios.Plugin ( runNagiosPlugin, addResult, CheckStatus (OK, Unknown, Warning, Critical), NagiosPlugin
                            , addPerfDatum, PerfValue (RealValue), UOM (NullUnit) )
import Options.Applicative
  ( execParser, info, header, progDesc, fullDesc, helper, Parser, option, long, short, metavar
  , value, help, str, auto, showDefault )
import Network.Simple.TCP (connect, send, recv)
import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Control.Monad.Loops (unfoldWhileM)
import qualified Data.Text as T
import Data.Ratio (approxRational)

import CgminerApi ( QueryApi (QueryApi), getTemps, decodeReply, Tempuratures)

data CliOptions = CliOptions
  { host :: String
  , port :: String
  , temp_warning :: Double
  , temp_error :: Double
  }

defaultTempWarningThreshold :: Double
defaultTempWarningThreshold = 90
defaultTempCriticalThreshold :: Double
defaultTempCriticalThreshold = 100

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

anyTempsAreZero :: Tempuratures -> Bool
anyTempsAreZero = any ((== 0) . snd)

anyTempsAboveThreshold :: Tempuratures -> Rational -> Bool
anyTempsAboveThreshold temps m = any ((>= m) . snd) temps

maximumTempThreshold :: Double
maximumTempThreshold = 120
minimumTempThreshold :: Double
minimumTempThreshold = 20

checkTemps :: Tempuratures -> Rational -> Rational -> NagiosPlugin ()
checkTemps temps tw tc = do
  let maxTemp = maximum $ snd <$> temps
  addResult OK $ "Max temp " <> (T.pack . show) (toDouble maxTemp) <> " C"

  if anyTempsAreZero temps
  then addResult Warning "At least one tempurature at 0 C"
  else return ()

  if anyTempsAboveThreshold temps tc
  then addResult Critical $ "Temperature exceeds critical threshold of " <> (T.pack . show) (toDouble tc) <> " C"
  else return ()

  if anyTempsAboveThreshold temps tw
  then addResult Warning $ "Temperature exceeds warning threshold of " <> (T.pack . show) (toDouble tw) <> " C"
  else return ()

  mapM_ (\temp -> do
      addTempData (fst temp) (snd temp)
    ) temps

  where
    addTempData :: T.Text -> Rational -> NagiosPlugin ()
    addTempData s t = addPerfDatum s (RealValue $ fromRational t) NullUnit
                                   (Just $ RealValue 20) (Just $ RealValue 120)
                                   (Just $ RealValue $ fromRational tw) (Just $ RealValue $ fromRational tc)
    toDouble :: Rational -> Double
    toDouble = fromRational

execCheck :: CliOptions -> IO ()
execCheck (CliOptions h p tw tc) = do
  r <- connect h p $ \(connectionSocket, _) -> do
    send connectionSocket $ toStrict . encode $ QueryApi "stats" "0"
    mconcat <$> unfoldWhileM ((/=) Nothing) (recv connectionSocket 4096)

  if r == Nothing
  then runNagiosPlugin $ addResult Unknown ( "Could not parse reply from "
                                          <> ((T.pack . show) h) <> ":" <> ((T.pack . show) p))
  else do
    let Just r' = fromStrict <$> r
    let Just (Right temps) = getTemps <$> decodeReply r'

    runNagiosPlugin $ checkTemps temps (approxRational tw 0.0001) (approxRational tc 0.0001)

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execCheck
  where
    opts = info (helper <*> cliOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based cgminer API returned values"
     <> header "check_cgminer - Nagios monitoring plugin for cgminer API" )
