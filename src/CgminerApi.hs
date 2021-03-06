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
                             , AntminerS17Vnish
                             )
              , Power (Watt), WorkMode (WorkMode))

data QueryApi = QueryApi
  { command :: Text
  , parameter :: Text
  } deriving (Generic, Show)
instance ToJSON QueryApi where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON QueryApi

data ReplyApi = ReplyApi
  { status  :: Maybe Value
  , stats   :: Maybe Array
  , summary :: Maybe Array
  } deriving (Generic, Show, Eq)
instance FromJSON ReplyApi where
  parseJSON = withObject "ReplyApi" $ \v -> ReplyApi
    <$> v .: "STATUS"
    <*> (v .:? "STATS")
    <*> (v .:? "SUMMARY")

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


-- | Parse `Stats` from STATS section of reply
getStats :: ReplyApi -> Either String Stats
getStats reply = flip parseEither reply $ \r -> do
  --let Just stats' = stats obj
  --fail $ show $ Data.Vector.length statsArray
  let Just s = stats r
      Just (Object infoStats) = s !? 0
      Just (Object rawStats) = s !? 1

  mMinerType <- infoStats .:? "Type"

  case mMinerType of
    Just (String "Antminer S9 SE") -> parseS9seStats AntminerS9SE rawStats
    Just (String "Antminer S9k") -> parseS9kStats AntminerS9k rawStats
    Just (String "Antminer S17 Pro") -> parseS17Stats AntminerS17Pro rawStats
    Just (String "Antminer S17") -> parseS17Stats AntminerS17 rawStats
    Just (String "Antminer S15") -> parseS15Stats AntminerS15 rawStats
    Just (String "Antminer DR5") -> parseDR5Stats AntminerDR5 rawStats
    Just (String "Antminer Z9-Mini") -> parseZ9miniStats AntminerZ9Mini rawStats
    Just (String "braiins-am1-s9") -> parseS9Stats AntminerS9 rawStats
    Just (String s') ->
      if "Antminer S17 (vnish" `isPrefixOf ` T.unpack s'
      then parseS17VnishStats AntminerS17Vnish rawStats
      else fail $ "Unexpected miner type: '" ++ T.unpack s' ++ "'"
    Just s' -> fail $ "Unexpected miner type: " ++ show s'
    -- Matches S9 miner case
    Nothing -> parseS9Stats AntminerS9 rawStats
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
