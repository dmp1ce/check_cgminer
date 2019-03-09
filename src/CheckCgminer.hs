{-|

Nagios monitoring plugin for cgminer API

-}

module CheckCgminer where

import System.Nagios.Plugin (runNagiosPlugin, addResult, CheckStatus (OK, Unknown, Warning), NagiosPlugin)
import Options.Applicative
  ( execParser, info, header, progDesc, fullDesc, helper, Parser, option, long, short, metavar
  , value, help, str )
import Network.Simple.TCP (connect, send, recv)
import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Control.Monad.Loops (unfoldWhileM)
import Data.Text (pack)

import CgminerApi ( QueryApi (QueryApi), getTemps, decodeReply, Tempuratures (Tempuratures))

data CliOptions = CliOptions
  { host :: String
  , port :: String }

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> option str
      ( long "host"
     <> short 'H'
     <> metavar "HOST"
     <> value "127.0.0.1"
     <> help "Hostname of cgminer API"
      )
  <*> option str
      ( long "port"
     <> short 'P'
     <> metavar "PORT"
     <> value "4028"
     <> help "Port of cgminer API"
      )

checkTemps :: Tempuratures -> NagiosPlugin ()
checkTemps (Tempuratures (s1_1, s1_2) (s2_1, s2_2) (s3_1, s3_2))
  |    s1_1 == 0 || s1_2 == 0
    || s2_1 == 0 || s2_2 == 0
    || s3_1 == 0 || s3_2 == 0 = addResult Warning "At least one tempurature at 0"
  | otherwise = addResult OK "No tempuratures at 0"

execCheck :: CliOptions -> IO ()
execCheck (CliOptions h p) = do
  r <- connect h p $ \(connectionSocket, _) -> do
    send connectionSocket $ toStrict . encode $ QueryApi "stats" "0"
    mconcat <$> unfoldWhileM ((/=) Nothing) (recv connectionSocket 4096)

  if r == Nothing
  then runNagiosPlugin $ addResult Unknown ( "Could not parse reply from "
                                          <> ((pack . show) h) <> ":" <> ((pack . show) p))
  else do
    let Just r' = fromStrict <$> r
    let Just (Right temps) = getTemps <$> decodeReply r'

    runNagiosPlugin $ checkTemps temps

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execCheck
  where
    opts = info (helper <*> cliOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based cgminer API returned values"
     <> header "check_cgminer - Nagios monitoring plugin for cgminer API" )
