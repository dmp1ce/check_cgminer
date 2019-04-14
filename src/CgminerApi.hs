module CgminerApi where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson ( ToJSON, FromJSON, toEncoding, genericToEncoding
                  , defaultOptions
                  , decode
                  , parseJSON, withObject, (.:),  Value(Number, Object, String), Array)
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
  (Number temp6) <- rawStats .: "temp6"
  (Number temp7) <- rawStats .: "temp7"
  (Number temp8) <- rawStats .: "temp8"
  (Number temp2_6) <- rawStats .: "temp2_6"
  (Number temp2_7) <- rawStats .: "temp2_7"
  (Number temp2_8) <- rawStats .: "temp2_8"
  (Number fan5) <- rawStats .: "fan5"
  (Number fan6) <- rawStats .: "fan6"
  (String chain_rate6_text) <- rawStats .: "chain_rate6"
  (String chain_rate7_text) <- rawStats .: "chain_rate7"
  (String chain_rate8_text) <- rawStats .: "chain_rate8"

  chain_rate6 <- textToRational chain_rate6_text
  chain_rate7 <- textToRational chain_rate7_text
  chain_rate8 <- textToRational chain_rate8_text

  return $ (Stats [ ("temp6", toRational temp6)
            , ("temp2_6", toRational temp2_6)
            , ("temp7", toRational temp7)
            , ("temp2_7", toRational temp2_7)
            , ("temp8", toRational temp8)
            , ("temp2_8", toRational temp2_8)
            ]
            [ ("chain_rate6", chain_rate6)
            , ("chain_rate7", chain_rate7)
            , ("chain_rate8", chain_rate8)
            ]
            [ ("fan5", toRational fan5)
            , ("fan6", toRational fan6)
            ]
           )
  where
    textToRational :: Text -> Parser Rational
    textToRational t =
      let e = (readEither $ T.unpack t) :: Either String Scientific
      in case e of
        Left _ -> fail $ "Failed to parse number: " ++ T.unpack t
        Right r -> return $ toRational r
