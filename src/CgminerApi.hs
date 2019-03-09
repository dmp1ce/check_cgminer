module CgminerApi where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding
                  , defaultOptions
                  , decode
                  , parseJSON, withObject, (.:),  Value(Number, Object), Array)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy (ByteString, stripSuffix)
import Data.Vector ( (!?) )

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

data Tempuratures = Tempuratures (Rational, Rational) (Rational, Rational) (Rational, Rational)
  deriving (Show, Eq)

-- | Decode reply if possible
decodeReply :: ByteString -> Maybe ReplyApi
decodeReply bs =
  let bss = stripSuffix "\NUL" bs
      decodeReply' :: Maybe ByteString -> Maybe ReplyApi
      decodeReply' (Just bs') = decodeReply bs'
      decodeReply' Nothing = decode bs
  in decodeReply' bss

-- | Parse tempuratures from reply
getTemps :: ReplyApi -> Either String Tempuratures
getTemps reply = flip parseEither reply $ \r -> do
  --let Just stats' = stats obj
  --fail $ show $ Data.Vector.length statsArray
  let Just s = stats r
      Just (Object rawStats) = s !? 1
  (Number temp6) <- rawStats .: "temp6"
  (Number temp7) <- rawStats .: "temp7"
  (Number temp8) <- rawStats .: "temp8"
  (Number temp2_6) <- rawStats .: "temp2_6"
  (Number temp2_7) <- rawStats .: "temp2_7"
  (Number temp2_8) <- rawStats .: "temp2_8"

  return $ Tempuratures (toRational temp6, toRational temp2_6)
                        (toRational temp7, toRational temp2_7)
                        (toRational temp8, toRational temp2_8)
