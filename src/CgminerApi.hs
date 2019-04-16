module CgminerApi where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding
                  , defaultOptions
                  , decode
                  , parseJSON, withObject, (.:),  Value(Number, Object, String), Array)
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
      Just (Object rawStats) = s !? 1
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
