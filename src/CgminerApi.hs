module CgminerApi where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding
                  , defaultOptions
                  , decode
                  , parseJSON, withObject, (.:), (.:?), Value(Number, Object, String), Array
                  , Object)
import Data.Aeson.Types (parseEither, Parser)
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
  { status  :: Maybe Value
  , stats   :: Maybe Array
  , summary :: Maybe Array
  } deriving (Generic, Show, Eq)
instance FromJSON ReplyApi where
  parseJSON = withObject "ReplyApi" $ \v -> ReplyApi
    <$> v .: "STATUS"
    <*> (v .:? "STATS")
    <*> (v .:? "SUMMARY")

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

-- | Parse stats from Summary section of reply
getSummary :: ReplyApi -> Either String Stats
getSummary reply = flip parseEither reply $ \r -> do
  let Just s = summary r
      --Just (Object infoStats) = s !? 0
      Just (Object rawStats) = s !? 0

  temps <- parseTextListToRational ["Temperature"] rawStats
  fans <- parseTextListToRational ["Fan Speed In","Fan Speed Out"] rawStats
  hrates <- parseTextListToRational ["MHS 5s"] rawStats
  return $ (Stats temps hrates fans)

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
    Just (String "Antminer S17 Pro") -> parseS17Stats rawStats
    Just (String "Antminer S17") -> parseS17Stats rawStats
    Just (String "Antminer S15") -> parseS15Stats rawStats
    Just (String "braiins-am1-s9") -> parseS9Stats rawStats
    Just (String s') -> fail $ "Unexpected miner type: '" ++ T.unpack s' ++ "'"
    Just s' -> fail $ "Unexpected miner type: " ++ show s'
    -- Matches S9 miner case
    Nothing -> parseS9Stats rawStats
  where


    parseS15Stats rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3","temp4"
                                       ,"temp2_1","temp2_2","temp2_3","temp2_4"
                                       ,"temp3_1","temp3_2","temp3_3","temp3_4"] rawStats
      fans <- parseTextListToRational ["fan1","fan2"] rawStats
      hrates <- parseTextListToRational ["chain_rate1","chain_rate2","chain_rate3","chain_rate4"] rawStats
      return $ (Stats temps hrates fans)
    parseS17Stats rawStats = do
      temps <- parseTextListToRational ["temp1","temp2","temp3"
                                       ,"temp2_1","temp2_2","temp2_3"
                                       ,"temp3_1","temp3_2","temp3_3"] rawStats
      fans <- parseTextListToRational ["fan1","fan2","fan3","fan4"] rawStats
      hrates <- parseTextListToRational ["chain_rate1","chain_rate2","chain_rate3"] rawStats
      return $ (Stats temps hrates fans)
    parseS9Stats rawStats = do
      temps <- parseTextListToRational ["temp6","temp2_6","temp7","temp2_7","temp8","temp2_8"] rawStats
      fans <- parseTextListToRational ["fan5","fan6"] rawStats
      hrates <- parseTextListToRational ["chain_rate6","chain_rate7","chain_rate8"] rawStats
      return $ (Stats temps hrates fans)

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
