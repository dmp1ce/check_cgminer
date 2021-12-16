module CgminerApi where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.List (isPrefixOf)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding
                  , defaultOptions
                  , decode
                  , parseJSON, withObject, (.:), (.:?), Value(Number, Object, String), Array
                  , Object)
import Data.Aeson.Types (parseEither, Parser)
import Data.ByteString.Lazy (ByteString, stripSuffix, toStrict, fromStrict)
import qualified Data.ByteString as BS
import Data.Vector ( (!?) )
import qualified Data.Text as T
import Text.Read (readEither)
import Data.Scientific (Scientific, toBoundedInteger)
import Helper ( miningDevicePowerConsumption
              , MiningDevice ( AntminerS9k, AntminerS9SE, AntminerS17Pro, AntminerS17
                             , AntminerS15, AntminerDR5, AntminerZ9Mini, AntminerS9
                             , AntminerS17Vnish, AntminerS17_BOSPlus
                             )
              , Power (Watt), WorkMode (WorkMode))
import qualified Data.HashMap.Strict as HM

data QueryApi = QueryApi
  { command :: Text
  , parameter :: Text
  } deriving (Generic, Show)
instance ToJSON QueryApi where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON QueryApi

data ReplyApi = ReplyApi
  { status  :: Maybe Array
  , stats   :: Maybe Array
  , summary :: Maybe Array
  , bos_temps :: Maybe [BOSPlusTemp]
  , bos_devs :: Maybe [BOSPlusDevs]
  , bos_fan ::  Maybe [BOSPlusFan]
  , bos_details ::  Maybe [BOSPlusDetails]
  } deriving (Generic, Show, Eq)
instance FromJSON ReplyApi where
  parseJSON = withObject "ReplyApi" $ \v -> do
    ReplyApi
      <$> v .:? "STATUS"
      <*> (v .:? "STATS")
      <*> (v .:? "SUMMARY")

      -- Created a subparser to handle Parsing the temp, hash and fan
      -- https://stackoverflow.com/questions/41207781/aeson-fromjson-with-nested-encoded-json
      <*> parse' v "temps" "TEMPS" --result -- maybe (return Nothing) parseJSON t'
      <*> parse' v "devs" "DEVS" -- return h_result
      <*> parse' v "fans" "FANS"   -- 'return f_result
      <*> parse' v "devdetails" "DEVDETAILS"   -- 'return f_result

    where
      parse' r' s1 s2 = do
        t <- r' .:? s1
        t' <- maybe HM.empty head t .:? s2
        maybe (return Nothing) parseJSON t'

data BOSPlusTemp = BOSPlusTemp
  { _bos_temp_temp :: Scientific
  , _bos_temp_id :: Scientific
  , _bos_temp_board :: Scientific
  , _bos_temp_chip :: Scientific
  } deriving (Generic, Show, Eq)
instance FromJSON BOSPlusTemp where
  parseJSON = withObject "BOSPlusTemp" $ \v -> BOSPlusTemp
    <$> v .: "TEMP"
    <*> (v .: "ID")
    <*> (v .: "Board")
    <*> (v .: "Chip")

data BOSPlusDevs = BOSPlusDevs
  { _bos_devs_av :: Scientific
  , _bos_devs_nominal :: Scientific
  , _bos_devs_id :: Scientific
  , _bos_devs_asc :: Scientific
  } deriving (Generic, Show, Eq)
instance FromJSON BOSPlusDevs where
  parseJSON = withObject "BOSPlusHash" $ \v -> BOSPlusDevs
    <$> v .: "MHS av"
    <*> v .: "Nominal MHS"
    <*> (v .: "ID")
    <*> (v .: "ASC")

data BOSPlusFan = BOSPlusFan
  { _bos_fan_rpm :: Scientific
  , _bos_fan_id :: Scientific
  } deriving (Generic, Show, Eq)
instance FromJSON BOSPlusFan where
  parseJSON = withObject "BOSPlusHash" $ \v -> BOSPlusFan
    <$> v .: "RPM"
    <*> (v .: "ID")

data BOSPlusDetails = BOSPlusDetails
  { _bos_details_id :: Scientific
  , _bos_details_voltage :: Scientific
  , _bos_details_frequency :: Scientific
  } deriving (Generic, Show, Eq)
instance FromJSON BOSPlusDetails where
  parseJSON = withObject "BOSPlusHash" $ \v -> BOSPlusDetails
    <$> v .: "DEVDETAILS"
    <*> (v .: "Voltage")
    <*> (v .: "Frequency")

data PowerStrategy = ConstantPower (Maybe Power)
                   -- | Watt per Gigahash
                   | DynamicPower (Maybe Power)
  deriving Show

data Stats = Stats { device :: Maybe MiningDevice
                   , power :: Maybe Power
                   , tempuratures :: TextRationalPairs
                   , hashrates :: TextRationalPairs
                   , hashratesIdeal :: TextRationalPairs
                   , hashrateIdealTotal :: Maybe Rational
                   , hashratesIdealPercentage :: Maybe Rational
                   , fanspeeds :: TextRationalPairs
                   , voltages    :: TextRationalPairs
                   , frequencies :: TextRationalPairs
                   , workMode :: Maybe WorkMode
                   }
  deriving (Eq, Show)
type TextRationalPairs = [TextRationalPair]
type TextRationalPair = (T.Text, Rational)

-- | Decode reply if possible
decodeReply :: ByteString -> Maybe ReplyApi
decodeReply bs =
  let bss = fixJSON bs
      decodeReply' :: Maybe ByteString -> Maybe ReplyApi
      decodeReply' (Just bs') = decodeReply bs'
      decodeReply' Nothing = decode bs
      fixJSON = (stripSuffix "\NUL" . fromStrict . replace "}{" "},{"  . toStrict)
  in decodeReply' bss

-- | For finding broken parts of JSON and fixing them
replace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace p p' b =
  let (f,s) = BS.breakSubstring p b
  in if BS.length s == 0 then f
     else replace p p' $ BS.concat [f, p', BS.drop (BS.length p) s]

-- | Parse stats from Summary section of reply
getSummary :: ReplyApi -> Either String Stats
getSummary reply = flip parseEither reply $ \r -> do
  let Just s = summary r
      --Just (Object infoStats) = s !? 0
      Just (Object rawStats) = s !? 0

  -- TODO: Get power consumption for device

  temps <- parseTextListToRational ["Temperature"] rawStats
  fans <- parseTextListToRational ["Fan Speed In","Fan Speed Out"] rawStats
  hrates <- parseTextListToRational ["MHS 5s"] rawStats
  return $ Stats Nothing Nothing temps hrates [] Nothing Nothing fans [] [] Nothing

-- | Parse all relevant BOS+ stats from the "temps+devs+fans+devdetails" API call
getBOSPlusStats :: ReplyApi -> Either String Stats
getBOSPlusStats reply = flip parseEither reply $ \r -> do
  let Just t = bos_temps r
      Just h = bos_devs r
      Just f = bos_fan r
      Just d = bos_details r

      -- Convert into a like of type [(String,Rational)] for parsing into Nagios checks
      concat' :: [a] -> (a -> Scientific)
        -> [(Text
            , a -> Scientific
            , Rational)] -- Correct the unit to what is expected by Stat datatype
        -> [(Text,Rational)]
      concat' bs id' ls' = concat $
        (\_t ->
           let Just _id = T.pack . show <$> (toBoundedInteger $ id' _t :: Maybe Int)
               lsF [] = []
               lsF ((s,f',a'):ls) = (s<>_id, toRational (f' _t)/a'):lsF ls
           in lsF ls'
        ) <$> bs

      temps = concat' t _bos_temp_temp [("Board",_bos_temp_board,1),("Chip", _bos_temp_chip,1)]
      hashes = concat' h _bos_devs_asc [("hash", _bos_devs_av,1000)]
      nominalHashes = concat' h _bos_devs_asc [("hashNominal", _bos_devs_nominal, 1000)]
      fanRPMs = concat' f _bos_fan_id [("fan", _bos_fan_rpm, 1)]
      _voltages = concat' d _bos_details_id [("voltage", _bos_details_voltage, 1)]
      _frequencies = concat' d _bos_details_id [("frequency", _bos_details_frequency, 1000000)]

  let nominalTotal = sum $ snd <$> nominalHashes

  return $ Stats Nothing Nothing temps hashes nominalHashes (Just nominalTotal) Nothing fanRPMs _voltages _frequencies Nothing

-- | Parse `Stats` from STATS section of reply
getStats :: ReplyApi -> Either String Stats
getStats reply = flip parseEither reply $ \r -> do
  --let Just stats' = stats obj
  --fail $ show $ Data.Vector.length statsArray
  let Just sStatus = status r
      Just (Object statusStats) = sStatus !? 0
      Just s = stats r
      Just (Object infoStats) = s !? 0
      Just (Object rawStats) = s !? 1

  -- Check for BOS+ firmware with "Description"
  mMinerDesc <- statusStats .:? "Description"
  -- Check for most other firmware with "Type"
  mMinerType <- infoStats .:? "Type"

  case (mMinerType, mMinerDesc) of
    (Just (String "Antminer S9 SE"), _) -> parseS9seStats AntminerS9SE rawStats
    (Just (String "Antminer S9k"), _) -> parseS9kStats AntminerS9k rawStats
    (Just (String "Antminer S17 Pro"), _) -> parseS17Stats AntminerS17Pro rawStats
    (Just (String "Antminer S17"), _) -> parseS17Stats AntminerS17 rawStats
    (Just (String "Antminer S15"), _) -> parseS15Stats AntminerS15 rawStats
    (Just (String "Antminer DR5"), _) -> parseDR5Stats AntminerDR5 rawStats
    (Just (String "Antminer Z9-Mini"), _) -> parseZ9miniStats AntminerZ9Mini rawStats
    (Just (String "braiins-am1-s9"), _) -> parseS9Stats AntminerS9 rawStats
    (Just (String s'), Just (String d)) -> parseBasedOnPrefix s' d rawStats
    (Just (String s'), Nothing) -> parseBasedOnPrefix s' "" rawStats
    (Nothing, Just (String d)) -> parseBasedOnPrefix "" d rawStats
    (Just s', _) -> fail $ "Unexpected miner type: " ++ show s'
    -- Default to S9 miner case
    (_, _) -> parseS9Stats AntminerS9 rawStats
  where
    parseStringRange :: Object -> Text -> [Int] -> Parser TextRationalPairs
    parseStringRange rawStats s range =
      parseTextListToRational (T.append s . T.pack . show <$> range) rawStats
    parseHashRatesAndIdeal :: Object -> [Int] -> Parser (TextRationalPairs, TextRationalPairs, Rational)
    parseHashRatesAndIdeal rawStats chainRateNums = do
      hrates <- parseStringRange rawStats "chain_rate" chainRateNums
      idealrates <- parseStringRange rawStats "chain_rateideal" chainRateNums
      idealrateTotal <- expectRational <$> rawStats .: "total_rateideal"
      return (hrates, idealrates, idealrateTotal)

    parseBasedOnPrefix :: Text -> Text -> Object -> Parser Stats
    parseBasedOnPrefix s d rawStats
      | "Antminer S17 (vnish" `isPrefixOf` T.unpack s = parseS17VnishStats AntminerS17Vnish rawStats
      | "BOSminer bosminer-plus" `isPrefixOf` T.unpack d = parseS17_BOSPlusStats AntminerS17_BOSPlus rawStats
      | otherwise = fail $ "Unexpected miner type: '" ++ T.unpack s ++ "'"

    parseS9seStats = parseS9kStats
    parseS9kStats d rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3"
                                       ,"temp2_1","temp2_2","temp2_3"] rawStats
      fans <- parseTextListToRational ["fan1","fan2"] rawStats
      (hrates,idealrates, idealrateTotal) <- parseHashRatesAndIdeal rawStats [1,2,3]
      return $ Stats (Just d) Nothing temps hrates idealrates (Just idealrateTotal) Nothing fans [] [] Nothing
    parseZ9miniStats d rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3"
                                       ,"temp2_1","temp2_2","temp2_3"] rawStats
      fans <- parseTextListToRational ["fan1"] rawStats
      hrates <- parseStringRange rawStats "chain_rate" [1,2,3]
      return $ Stats (Just d) Nothing temps hrates [] Nothing Nothing fans [] [] Nothing
    parseDR5Stats d rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3"
                                       ,"temp2_1","temp2_2","temp2_3"] rawStats
      fans <- parseTextListToRational ["fan1","fan2"] rawStats
      hrates <- parseStringRange rawStats "chain_rate" [1,2,3]
      return $ Stats (Just d) Nothing temps hrates [] Nothing Nothing fans [] [] Nothing
    parseS15Stats d rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3","temp4"
                                       ,"temp2_1","temp2_2","temp2_3","temp2_4"
                                       ,"temp3_1","temp3_2","temp3_3","temp3_4"] rawStats
      fans <- parseTextListToRational ["fan1","fan2"] rawStats
      (hrates,idealrates, idealrateTotal) <- parseHashRatesAndIdeal rawStats [1,2,3,4]
      return $ Stats (Just d) Nothing temps hrates idealrates (Just idealrateTotal) Nothing fans [] [] Nothing
    parseS17Stats d rawStats = do
      (temps, fans, hrates, idealrates, idealrateTotal) <- s17ParseStats' rawStats
      mode <- parseWorkMode rawStats
      return $ Stats (Just d) Nothing temps hrates idealrates (Just idealrateTotal) Nothing fans [] [] mode
    parseS17VnishStats d rawStats = do
      (temps, fans, hrates, idealrates, idealrateTotal) <- s17ParseStats' rawStats
      return $ Stats (Just d) Nothing temps hrates idealrates (Just idealrateTotal) Nothing fans [] [] Nothing
    parseS9Stats d rawStats = do
      temps <- parseTextListToRational ["temp6","temp2_6","temp7","temp2_7","temp8","temp2_8"] rawStats
      fans <- parseTextListToRational ["fan5","fan6"] rawStats
      (hrates,idealrates, idealrateTotal) <- parseHashRatesAndIdeal rawStats [6,7,8]
      volts <- parseTextListToRational ["voltage6","voltage7","voltage8"] rawStats
      freqs <- parseTextListToRational ["freq_avg6","freq_avg7","freq_avg8"] rawStats
      return $ Stats (Just d) Nothing temps hrates idealrates (Just idealrateTotal) Nothing fans volts freqs Nothing
    parseS17_BOSPlusStats d _ =
      -- Skip parsing because nothing of interest will be available in stats for BOS+ miners
      return $ Stats (Just d) Nothing [] [] [] Nothing Nothing [] [] [] Nothing

    s17ParseStats' rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3"
                                       ,"temp2_1","temp2_2","temp2_3"
                                       ,"temp3_1","temp3_2","temp3_3"] rawStats
      fans <- parseTextListToRational ["fan1","fan2","fan3","fan4"] rawStats
      (hrates,idealrates, idealrateTotal) <- parseHashRatesAndIdeal rawStats [1,2,3]
      return (temps,fans,hrates,idealrates,idealrateTotal)

-- | Update Stats with power consumption.
--   The power consumption can be calculated as a constant or dynamic power
--   Dynamic power is based off of hash rate
--   If the power cannot be calculated then nothing is changed in the statistics.
includePowerConsumption :: PowerStrategy -> Stats -> Stats
includePowerConsumption ps s = s {power = getPowerConsumption ps s}

getPowerConsumption :: PowerStrategy -> Stats -> Maybe Power
getPowerConsumption (DynamicPower Nothing) (Stats (Just AntminerS17Pro) _ _ h _ _ _ _ _ _ _) =
  -- TODO: Should move hard coded values to miningDevicePowerConsumption function
  -- 45W per TH
  Just $ Watt $ sum (snd <$> h) * 0.045
getPowerConsumption (DynamicPower (Just (Watt w))) (Stats _ _ _ h _ _ _ _ _ _ _) =
  Just $ Watt $ sum (snd <$> h) * w
getPowerConsumption (ConstantPower Nothing) (Stats (Just d) _ _ _ _ _ _ _ _ _ m) =
  eitherToMaybe $ miningDevicePowerConsumption d m
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Right b) = Just b
    eitherToMaybe (Left _) = Nothing
getPowerConsumption (ConstantPower mp) _ = mp
getPowerConsumption _ _ = Nothing

includeIdealPercentage :: Stats -> Stats
includeIdealPercentage s = s {hashratesIdealPercentage = calcIdealPercentage s}

-- Try to get the percentage of the ideal rate based on the current hash rate
calcIdealPercentage :: Stats -> Maybe Rational
calcIdealPercentage (Stats _ _ _ hr hri hriTotal _ _ _ _ _) =
  -- Make sure an ideal hashrate exists to avoid division error
  let is = sum (snd <$> hri)
      isValid = is > 0
      hs = sum (snd <$> hr)
  in if isValid
        then Just $ hs / is
        else if ((> 0) <$> hriTotal) == Just True
                then (hs /) <$> hriTotal
                else Nothing

-- We really want a rational from the data so make it happen here.
expectRational :: Value -> Rational
expectRational (Number n) = toRational n
expectRational (String s) = textToRational s
expectRational e = error $ "Could not parse " ++ show e
textToRational :: Text -> Rational
textToRational t =
  let e = (readEither $ T.unpack t) :: Either String Scientific
  in case e of
    Left s -> if t == ""
              then 0
              else error $ s ++ "\nFailed to parse number: '" ++ T.unpack t ++ "'"
    Right r -> toRational r

-- From a list of keys get the expected rational
parseTextListToRational :: [Text] -> Object -> Parser TextRationalPairs
parseTextListToRational ts s = mapM (\t -> do
                                   x <- expectRational <$> s .: t
                                   return (t, x)
                                   ) ts

parseWorkMode :: Object -> Parser (Maybe WorkMode)
parseWorkMode s = do
  wm <- s .: "Mode"
  return $ case wm of
    (Number n) -> WorkMode <$> toBoundedInteger n
    _ -> Nothing

-- Detects BraiinsOS+ miner
isBOSMinerPlus :: Stats -> Bool
isBOSMinerPlus (Stats (Just AntminerS17_BOSPlus) _ _ _ _ _ _ _ _ _ _) = True
isBOSMinerPlus _ = False
