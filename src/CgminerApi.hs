module CgminerApi where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding
                  , defaultOptions
                  , decode
                  , parseJSON, withObject, (.:), (.:?), Value(Number, Object, String), Array)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy (ByteString, stripSuffix)
import Data.Vector ( (!?) )
import qualified Data.Text as T
import Text.Read (readEither)
import Data.Scientific (Scientific)

data QueryApi = QueryApi
  { command :: Text
  , parameter :: Text
  } deriving (Generic, Show)
instance ToJSON QueryApi where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON QueryApi

data ReplyApi = ReplyApi
  { status :: Maybe Value
  , stats :: Maybe Array
  } deriving (Generic, Show, Eq)
instance FromJSON ReplyApi where
  parseJSON = withObject "ReplyApi" $ \v -> ReplyApi
    <$> v .: "STATUS"
    <*> v .: "STATS"

data Stats = Stats { tempuratures :: TextRationalPairs
                   , hashrates :: TextRationalPairs
                   , fanspeeds :: TextRationalPairs
                   }
  deriving (Eq, Show)
type TextRationalPairs = [TextRationalPair]
type TextRationalPair = (T.Text, Rational)

-- | Decode reply if possible
decodeReply :: ByteString -> Maybe ReplyApi
decodeReply bs =
  let bss = stripSuffix "\NUL" bs
      decodeReply' :: Maybe ByteString -> Maybe ReplyApi
      decodeReply' (Just bs') = decodeReply bs'
      decodeReply' Nothing = decode bs
  in decodeReply' bss

-- | Parse temperatures from reply
getStats :: ReplyApi -> Either String Stats
getStats reply = flip parseEither reply $ \r -> do
  --let Just stats' = stats obj
  --fail $ show $ Data.Vector.length statsArray
  let Just s = stats r
      Just (Object infoStats) = s !? 0
      Just (Object rawStats) = s !? 1

  mMinerType <- infoStats .:? "Type"

  case mMinerType of
    Just (String "Antminer S17 Pro") -> do
      temp1 <- expectRational <$> rawStats .: "temp1"
      temp2 <- expectRational <$> rawStats .: "temp2"
      temp3 <- expectRational <$> rawStats .: "temp3"
      temp2_1 <- expectRational <$> rawStats .: "temp2_1"
      temp2_2 <- expectRational <$> rawStats .: "temp2_2"
      temp2_3 <- expectRational <$> rawStats .: "temp2_3"
      temp3_1 <- expectRational <$> rawStats .: "temp3_1"
      temp3_2 <- expectRational <$> rawStats .: "temp3_2"
      temp3_3 <- expectRational <$> rawStats .: "temp3_3"
      fan1 <- expectRational <$> rawStats .: "fan1"
      fan2 <- expectRational <$> rawStats .: "fan2"
      fan3 <- expectRational <$> rawStats .: "fan3"
      fan4 <- expectRational <$> rawStats .: "fan4"
      chain_rate1 <- expectRational <$> rawStats .: "chain_rate1"
      chain_rate2 <- expectRational <$> rawStats .: "chain_rate2"
      chain_rate3 <- expectRational <$> rawStats .: "chain_rate3"
      return $ (Stats
                [ ("temp1", temp1)
                , ("temp2", temp2)
                , ("temp3", temp3)
                , ("temp2_1", temp2_1)
                , ("temp2_2", temp2_2)
                , ("temp2_3", temp2_3)
                , ("temp3_1", temp3_1)
                , ("temp3_2", temp3_2)
                , ("temp3_3", temp3_3)
                ]
                [ ("chain_rate1", chain_rate1)
                , ("chain_rate2", chain_rate2)
                , ("chain_rate3", chain_rate3)
                ]
                [ ("fan1", fan1)
                , ("fan2", fan2)
                , ("fan3", fan3)
                , ("fan4", fan4)
                ]
               )
    Just (String "braiins-am1-s9") -> parseS9Stats rawStats
    Just s' -> fail $ "Unexpected miner type: " ++ show s'
    -- Matches S9 miner case
    Nothing -> parseS9Stats rawStats
  where
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
    parseS9Stats rawStats = do
      temp6 <- expectRational <$> rawStats .: "temp6"
      temp7 <- expectRational <$> rawStats .: "temp7"
      temp8 <- expectRational <$> rawStats .: "temp8"
      temp2_6 <- expectRational <$> rawStats .: "temp2_6"
      temp2_7 <- expectRational <$> rawStats .: "temp2_7"
      temp2_8 <- expectRational <$> rawStats .: "temp2_8"
      fan5 <- expectRational <$> rawStats .: "fan5"
      fan6 <- expectRational <$> rawStats .: "fan6"
      chain_rate6 <- expectRational <$> rawStats .: "chain_rate6"
      chain_rate7 <- expectRational <$> rawStats .: "chain_rate7"
      chain_rate8 <- expectRational <$> rawStats .: "chain_rate8"

      return $ (Stats [ ("temp6", temp6)
               , ("temp2_6", temp2_6)
               , ("temp7", temp7)
               , ("temp2_7", temp2_7)
               , ("temp8", temp8)
               , ("temp2_8", temp2_8)
               ]
               [ ("chain_rate6", chain_rate6)
               , ("chain_rate7", chain_rate7)
               , ("chain_rate8", chain_rate8)
               ]
               [ ("fan5", fan5)
               , ("fan6", fan6)
               ]
               )
