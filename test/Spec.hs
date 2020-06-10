import ReplyExamples
import Helper ( WorkMode (WorkMode), CacheUTCTime (CacheUTCTime)
              , CacheData(CacheData), TimeUnit (Second, Day)
              , MonetaryUnit(USD), Rate (Rate), deleteCache, cacheIO
              , delayedCacheIO, Price (Price), Bitcoins (Bitcoins)
              , BitcoinUnit (Bitcoin), Difficulty (Difficulty), HashRates (Ghs)
              , revenueRate, Time (Time), calculateTimeToGenerateBlock
              , getProfitability, EnergyUnit (KiloWattHour)
              , EnergyRate (EnergyRate), Power (Watt), MiningDevice (..) )
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), (@?), assertBool )

import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode)
import qualified Data.Serialize as S
import qualified Data.Time.Clock as C

import CgminerApi ( QueryApi (QueryApi), decodeReply, getStats, getSummary, Stats (Stats), replace
                  , includePowerConsumption, calcIdealPercentage, includeIdealPercentage, PowerStrategy (..) )
import CheckCgminer (anyTempsAreZero, anyAboveThreshold, anyBelowThreshold)
import qualified Data.Text as T

-- Thread testing
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar, readMVar)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [json, checks, misc, cache]

exampleSummaryCommand :: ByteString
exampleSummaryCommand = "{\"command\":\"summary\",\"parameter\":\"0\"}"

json :: TestTree
json = testGroup "json tests"

  [ testCase "encode QueryApi should match example command text" $
    encode (QueryApi "summary" "0") @?= exampleSummaryCommand
  , testCase "Successfully decode example reply (braiinsOS s9)" $
    isJust (decodeReply exampleReplyS9BraiinsOS) @? "exampleReply could not be decoded"
  , testCase "Can get stats (braiinsOS s9)" $
      let Just x = decodeReply exampleReplyS9BraiinsOS
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerS9) (Just $ Watt 1323)
                           [ ("temp6"::T.Text, 59::Rational)
                           , ("temp2_6"::T.Text, 75)
                           , ("temp7", 54)
                           , ("temp2_7", 71)
                           , ("temp8", 55)
                           , ("temp2_8", 74)
                           ]
                           [ ("chain_rate6",4524.28)
                           , ("chain_rate7",4679.16)
                           , ("chain_rate8",4550.58)
                           ]
                           [ ("chain_rateideal6",4525.53)
                           , ("chain_rateideal7",4699.58)
                           , ("chain_rateideal8",4530.58)
                           ]
                           (Just 13755.69)
                           (Just $ 1375402 % 1375569) -- 99.987859569 %
                           [ ("fan5",5040)
                           , ("fan6",3600)
                           ]
                           [ ("voltage6",8.9)
                           , ("voltage7",8.7)
                           , ("voltage8",8.9)
                           ]
                           [ ("freq_avg6",631)
                           , ("freq_avg7",656)
                           , ("freq_avg8",631)
                           ]
                           Nothing
                          )
  , testCase "Successfully decode example reply (Z9-mini)" $
    isJust (decodeReply exampleReplyZ9mini) @? "exampleReply could not be decoded"
  , testCase "Can get stats (Z9-mini)" $
      let Just x = decodeReply exampleReplyZ9mini
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerZ9Mini) Nothing
                           [ ("temp1"::T.Text, 54::Rational)
                           , ("temp2"::T.Text, 53)
                           , ("temp3", 56)
                           , ("temp2_1", 69)
                           , ("temp2_2", 68)
                           , ("temp2_3", 71)
                           ]
                           [ ("chain_rate1",5.07)
                           , ("chain_rate2",5.14)
                           , ("chain_rate3",5.61)
                           ]
                           []
                           Nothing
                           Nothing
                           [ ("fan1",4200)]
                           []
                           []
                           Nothing
                          )
  , testCase "Successfully decode example reply (S9k)" $
    isJust (decodeReply exampleReplyS9k) @? "exampleReply could not be decoded"
  , testCase "Can get stats (S9k)" $
      let Just x = decodeReply exampleReplyS9k
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerS9k) Nothing
                           [ ("temp1"::T.Text, 63::Rational)
                           , ("temp2"::T.Text, 62)
                           , ("temp3", 62)
                           , ("temp2_1", 87)
                           , ("temp2_2", 84)
                           , ("temp2_3", 90)
                           ]
                           [ ("chain_rate1",4375.93)
                           , ("chain_rate2",4529.85)
                           , ("chain_rate3",4090.06)
                           ]
                           [ ("chain_rateideal1",0)
                           , ("chain_rateideal2",0)
                           , ("chain_rateideal3",0)
                           ]
                           (Just 14000)
                           (Just $ 20306 % 21875) -- 92.827428571 %
                           [ ("fan1",6480)
                           , ("fan2",6480)]
                           []
                           []
                           Nothing
                          )
  , testCase "Successfully decode example reply (S9se)" $
    isJust (decodeReply exampleReplyS9se) @? "exampleReply could not be decoded"
  , testCase "Can get stats (S9se)" $
      let Just x = decodeReply exampleReplyS9se
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerS9SE) (Just $ Watt 1280)
                           [ ("temp1"::T.Text, 60::Rational)
                           , ("temp2"::T.Text, 61)
                           , ("temp3", 62)
                           , ("temp2_1", 79)
                           , ("temp2_2", 79)
                           , ("temp2_3", 79)
                           ]
                           [ ("chain_rate1",5123.58)
                           , ("chain_rate2",5057.61)
                           , ("chain_rate3",5475.41)
                           ]
                           [ ("chain_rateideal1",0)
                           , ("chain_rateideal2",0)
                           , ("chain_rateideal3",0)
                           ]
                           (Just 16000)
                           (Just $ 78283 % 80000) -- 97.85375 %
                           [ ("fan1",1920)
                           , ("fan2",1920)]
                           []
                           []
                           Nothing
                          )
  , testCase "Successfully decode example reply (DR5)" $
    isJust (decodeReply exampleReplyDR5) @? "exampleReply could not be decoded"
  , testCase "Can get stats (DR5)" $
      let Just x = decodeReply exampleReplyDR5
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerDR5) Nothing
                           [ ("temp1"::T.Text, 62::Rational)
                           , ("temp2"::T.Text, 66)
                           , ("temp3", 60)
                           , ("temp2_1", 69)
                           , ("temp2_2", 75)
                           , ("temp2_3", 68)
                           ]
                           [ ("chain_rate1",11577.54)
                           , ("chain_rate2",11956.85)
                           , ("chain_rate3",12033.82)
                           ]
                           []
                           Nothing
                           Nothing
                           [ ("fan1",3600)
                           , ("fan2",3600)
                           ][][] Nothing
                          )
  , testCase "Successfully decode example reply (s15)" $
    isJust (decodeReply exampleReplyS15) @? "exampleReply could not be decoded"
  , testCase "Can get stats (stock s15)" $
      let Just x = decodeReply exampleReplyS15
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerS15) (Just $ Watt 1596)
                           [ ("temp1"::T.Text, 60::Rational)
                           , ("temp2"::T.Text, 65)
                           , ("temp3", 66)
                           , ("temp4", 59)
                           , ("temp2_1", 55)
                           , ("temp2_2", 58)
                           , ("temp2_3", 58)
                           , ("temp2_4", 55)
                           , ("temp3_1", 75)
                           , ("temp3_2", 80)
                           , ("temp3_3", 80)
                           , ("temp3_4", 74)
                           ]
                           [ ("chain_rate1",7278.55)
                           , ("chain_rate2",6948.71)
                           , ("chain_rate3",6860.75)
                           , ("chain_rate4",7366.51)
                           ]
                           [ ("chain_rateideal1",0)
                           , ("chain_rateideal2",0)
                           , ("chain_rateideal3",0)
                           , ("chain_rateideal4",0)
                           ]
                           (Just 28000)
                           (Just $ 711363 % 700000) -- 101.623285714 %
                           [ ("fan1",4080)
                           , ("fan2",4200)
                           ][][] Nothing
                          )
  , testCase "Successfully decode example reply (s17 Pro)" $
    isJust (decodeReply exampleReplyS17Pro) @? "exampleReply could not be decoded"
  , testCase "Can get stats (stock s17)" $
      let Just x = decodeReply exampleReplyS17Pro
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerS17Pro) (Just $ Watt 2094)
                           [ ("temp1"::T.Text, 63::Rational)
                           , ("temp2"::T.Text, 63)
                           , ("temp3", 61)
                           , ("temp2_1", 63)
                           , ("temp2_2", 63)
                           , ("temp2_3", 62)
                           , ("temp3_1", 78)
                           , ("temp3_2", 80)
                           , ("temp3_3", 76)
                           ]
                           [ ("chain_rate1",16558.24)
                           , ("chain_rate2",16954.05)
                           , ("chain_rate3",16514.26)
                           ]
                           [ ("chain_rateideal1",16911)
                           , ("chain_rateideal2",16911)
                           , ("chain_rateideal3",16911)
                           ]
                           (Just 50000)
                           (Just $ 1000531 % 1014660) -- 98.607513847%
                           [ ("fan1",2760)
                           , ("fan2",2760)
                           , ("fan3",3720)
                           , ("fan4",3600)
                           ][][] (Just $ WorkMode 1)
                          )
  , testCase "Successfully decode example reply (s17 Vnish)" $
    isJust (decodeReply exampleReplyS17Vnish) @? "exampleReply could not be decoded"
  , testCase "Can get stats (s17 vnish)" $
      let Just x = decodeReply exampleReplyS17Vnish
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getStats x)
         @?= Right ( Stats (Just AntminerS17Vnish) (Just $ Watt 2800)
                           [ ("temp1"::T.Text, 55::Rational)
                           , ("temp2"::T.Text, 57)
                           , ("temp3", 56)
                           , ("temp2_1", 75)
                           , ("temp2_2", 75)
                           , ("temp2_3", 75)
                           , ("temp3_1", 75)
                           , ("temp3_2", 75)
                           , ("temp3_3", 75)
                           ]
                           [ ("chain_rate1",21765.56)
                           , ("chain_rate2",21759.19)
                           , ("chain_rate3",21635.21)
                           ]
                           [ ("chain_rateideal1",21744)
                           , ("chain_rateideal2",21744)
                           , ("chain_rateideal3",21744)
                           ]
                           (Just 65232)
                           (Just $ 1628999 % 1630800) -- 99.889563404 %
                           [ ("fan1",0)
                           , ("fan2",0)
                           , ("fan3",0)
                           , ("fan4",0)
                           ][][] Nothing
                          )
  , testCase "Successfully decode example reply (Whatsminer)" $
    isJust (decodeReply exampleReplyWhatsminer) @? "exampleReply could not be decoded"
  , testCase "Can get stats (whatsminer)" $
      let Just x = decodeReply exampleReplyWhatsminer
      in (includeIdealPercentage . includePowerConsumption (ConstantPower Nothing) <$> getSummary x)
         @?= Right ( Stats Nothing Nothing
                           [ ("Temperature"::T.Text, 75.00::Rational)]
                           [ ("MHS 5s",35523530.10)] []
                           Nothing Nothing
                           [ ("Fan Speed In",6360)
                           , ("Fan Speed Out",6390)
                           ][][] Nothing
                            )
  ]

checks :: TestTree
checks = testGroup "checks to perform tests"
  [ testCase "Detect temperatures when at zero"
    (anyTempsAreZero [("", 0),("",55),("",5.5),("",33), ("",0),("",34)] @?= True)
  , testCase "Don't detect zero when temperatures NOT at zero"
    (anyTempsAreZero [("",1),("",55),("",5.5),("",33), ("",2), ("",34)] @?= False)
  , testCase "Don't detect temps when below threshold"
    (anyAboveThreshold [("",1),("",55)] (10000.0::Rational) @?= False)
  , testCase "Detect temps when above threshold"
    (anyAboveThreshold [("",1),("",55),("",200)] 100 @?= True)
  , testCase "Detect hashrates when below threshold" $
    anyBelowThreshold [("", 10), ("",100), ("",44)] 50 @?= True
  , testCase "Don't detect hashrates when above threshold" $
    anyBelowThreshold [("", 10), ("",100), ("",44)] 2 @?= False
  ]

misc :: TestTree
misc = testGroup "Other testable functionality"
  [ testCase "Replace lazy ByteString substring"
    (replace  "12345" "" "Hello 1234512345World" @?= "Hello World")
  , testCase "Profitability is greater than 0" $
    let (Rate USD Day p) = getProfitability (Ghs [57000]) (Difficulty 120033340651.237000)
            (Bitcoins Bitcoin 12.5) (Bitcoins Bitcoin 0)
            (Watt 2500) (EnergyRate USD KiloWattHour 0.09) (Price USD 10000) 0
    in p >= 0
    @? ("Profitability was less than 0. Profitability is " ++ show (fromRational p :: Double) ++ " per second.")
  , testCase "Revenue doubles with price doubling" $
    let price' = 100000
        difficulty = Difficulty 120033340651.237000
        hashes = Ghs [1000,1000]
        reward = Bitcoins Bitcoin 25
        fees = Bitcoins Bitcoin 0
        price = Price USD price'
        price2 = Price USD (2*price')
        Rate USD Second revenue1 = revenueRate hashes difficulty reward fees price
        Rate USD Second revenue2 = revenueRate hashes difficulty reward fees price2
    in ((revenue1*2) == revenue2) @? "Revenue 1: (" ++ show (fromRational revenue1 :: Double) ++
       ") Revenue 2: (" ++ show (fromRational revenue2 :: Double) ++ ")"
  , testCase "Known time to generate block from example (about 23.86092 days at 1 Ghs at 20000 difficulty)" $
    calculateTimeToGenerateBlock (Ghs [1]) (Difficulty 20000) @?= Just (Time Second (268435456 / 3125))
  , testCase "Revenue is positive" $
    let Rate USD Second x = revenueRate (Ghs [1]) (Difficulty 20000) (Bitcoins Bitcoin 25) (Bitcoins Bitcoin 0) (Price USD 10000)
    in x >= 0 @? "Revenue is less than 0"
  , testCase "includePowerConsumption - simple case" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [] Nothing Nothing [] [] [] Nothing
    in includePowerConsumption (DynamicPower Nothing) s @?= Stats (Just AntminerS17Pro) (Just (Watt 45)) []
       [("",500), ("",500)] [] Nothing Nothing [] [] [] Nothing
  , testCase "calcIdealDifference - simple case" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [("",1000),("",1000)] Nothing Nothing [] [] []  Nothing
    in calcIdealPercentage s @?= Just 0.5
  , testCase "calcIdealDifference - total ideal" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [] (Just 2000) Nothing [] [] []  Nothing
    in calcIdealPercentage s @?= Just 0.5

  , testCase "calcIdealDifference - invalid case #1" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [] Nothing Nothing [] [] []  Nothing
    in calcIdealPercentage s @?= Nothing
  , testCase "calcIdealDifference - invalid case #2" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [("",0)] Nothing Nothing [] [] []  Nothing
    in calcIdealPercentage s @?= Nothing
  , testCase "calcIdealDifference - invalid case #3" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [("",0)] (Just 0) Nothing [] [] []  Nothing
    in calcIdealPercentage s @?= Nothing
  , testCase "includePowerConsumption - dynamic power specified" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [] Nothing Nothing [] [] [] Nothing
    in includePowerConsumption (DynamicPower (Just $ Watt 0.01)) s @?= Stats (Just AntminerS17Pro) (Just (Watt 10)) []
       [("",500), ("",500)] [] Nothing Nothing [] [] [] Nothing
  , testCase "includePowerConsumption - constant power specified" $
    let s = Stats (Just AntminerS17Pro) Nothing [] [("",500),("",500)] [] Nothing Nothing [] [] [] Nothing
    in includePowerConsumption (ConstantPower (Just $ Watt 1000)) s @?= Stats (Just AntminerS17Pro) (Just (Watt 1000)) []
       [("",500), ("",500)] [] Nothing Nothing [] [] [] Nothing

    ]


cache :: TestTree
cache = testGroup "Cache tests"
  [ testCase "CacheUTCTime can be encoded and decoded" $ do
    t <- C.getCurrentTime
    let Right (CacheUTCTime t') = S.decode $ S.encode $ CacheUTCTime t
    assertBool "decode . encode didn't work for CacheUTCTime" (t' == t)
  , testCase "CacheData can be encoded and decoded" $ do
    t <- C.getCurrentTime
    let out = "Some IO data" :: String
        Right (CacheData (CacheUTCTime t') out') = S.decode $ S.encode $ CacheData (CacheUTCTime t) out
    assertBool "decode . encode didn't work for CacheData" (out' == out && t' == t)
  , testCase "cacheIO respects cache time" $ do
    let m1 = "Something here"
        m2 = "Something here but won't get"
        c = "cacheIO_test1"
    out1 <- cacheIO c C.nominalDay (pure m1::IO String)
    out2 <- cacheIO c C.nominalDay (pure m2::IO String)
    deleteCache c
    assertBool "cacheIO didn't respect cache time" (out1 == m1 && out2 == m1)
  , testCase "delayedCacheIO respects cache time" $ do
    let m1 = "Something here"
        m2 = "Something here but won't get"
        c = "delayedCacheIO_test1"
    out1 <- delayedCacheIO c C.nominalDay 5 (pure m1::IO String)
    out2 <- delayedCacheIO c C.nominalDay 5 (pure m2::IO String)
    deleteCache c
    assertBool "delayedCacheIO didn't respect cache time" (out1 == m1 && out2 == m1)
  , testCase "cacheIO can be invalidated" $ do
    let m1 = "Something here"
        m2 = "Something here more"
        c = "cacheIO_test2"
    out1 <- cacheIO c C.nominalDay (pure m1::IO String)
    out2 <- cacheIO c 0 (pure m2::IO String)
    deleteCache c
    assertBool "cacheIO didn't respect cache time" (out1 == m1 && out2 == m2)
  , testCase "delayedCacheIO waits for previous processes correctly" $ do
    let m1 = "p1 "
        m2 = "p2 "
        m3 = "p3 "
        halfSecond = 500000
        c = "cacheIO_test3"
    mv <- newEmptyMVar

    -- Start a delayed cacheIO with 1 second cache and 3 second delay if currently running
    _ <- forkIO $ delayedCacheIO c 1 15 $ do
      threadDelay $ halfSecond * 2
      putMVar mv m1

    -- Wait half a second and then run another delayed cacheIO and see if it
    -- waits for the last delayed cacheIO
    threadDelay halfSecond
    -- This will not run because it will hit the cache
    _ <- forkIO $ delayedCacheIO c 1 15 $ do
      t <- takeMVar mv
      putMVar mv (t ++ m2)

    -- Let the cache expire
    threadDelay $ halfSecond * 4
    _ <- forkIO $ delayedCacheIO c 1 15 $ do
      t <- takeMVar mv
      putMVar mv (t ++ m3)

    -- Wait for processes to complete
    threadDelay halfSecond

    out <- readMVar mv

    deleteCache c
    assertBool ("delayedCacheIO didn't complete in the correct order. Output: " ++ out) (out == (m1 ++ m3) )
  ]
