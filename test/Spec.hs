import ReplyExamples
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), (@?) )

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode)

import CgminerApi ( QueryApi (QueryApi), decodeReply, getStats, getSummary, Stats (Stats), replace
                  , WorkMode (WorkMode) )
import CheckCgminer (anyTempsAreZero, anyAboveThreshold, anyBelowThreshold)
import qualified Data.Text as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [json, checks, misc]

exampleSummaryCommand :: ByteString
exampleSummaryCommand = "{\"command\":\"summary\",\"parameter\":\"0\"}"

json :: TestTree
json = testGroup "json tests"

  [ testCase "encode QueryApi should match example command text" $
    (encode $ QueryApi "summary" "0") @?= exampleSummaryCommand
  , testCase "Successfully decode example reply (braiinsOS s9)" $
    ((decodeReply exampleReplyS9BraiinsOS) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get stats (braiinsOS s9)" $
      let Just x = decodeReply exampleReplyS9BraiinsOS
      in (getStats x) @?= (Right $ Stats
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
    ((decodeReply exampleReplyZ9mini) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get stats (Z9-mini)" $
      let Just x = decodeReply exampleReplyZ9mini
      in (getStats x) @?= (Right $ Stats
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
                           [ ("fan1",4200)]
                           []
                           []
                           Nothing
                          )
  , testCase "Successfully decode example reply (DR5)" $
    ((decodeReply exampleReplyDR5) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get stats (DR5)" $
      let Just x = decodeReply exampleReplyDR5
      in (getStats x) @?= (Right $ Stats
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
                           [ ("fan1",3600)
                           , ("fan2",3600)
                           ][][] Nothing
                          )
  , testCase "Successfully decode example reply (s15)" $
    ((decodeReply exampleReplyS15) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get stats (stock s15)" $
      let Just x = decodeReply exampleReplyS15
      in (getStats x) @?= (Right $ Stats
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
                           [ ("fan1",4080)
                           , ("fan2",4200)
                           ][][] Nothing
                          )
  , testCase "Successfully decode example reply (s17)" $
    ((decodeReply exampleReplyS17) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get stats (stock s17)" $
      let Just x = decodeReply exampleReplyS17
      in (getStats x) @?= (Right $ Stats
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
                           [ ("fan1",2760)
                           , ("fan2",2760)
                           , ("fan3",3720)
                           , ("fan4",3600)
                           ][][] (Just $ WorkMode 1)
                          )
  , testCase "Successfully decode example reply (Whatsminer)" $
    ((decodeReply exampleReplyWhatsminer) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get stats (whatsminer)" $
      let Just x = decodeReply exampleReplyWhatsminer
      in (getSummary x) @?= (Right $ Stats
                           [ ("Temperature"::T.Text, 75.00::Rational)]
                           [ ("MHS 5s",35523530.10)]
                           [ ("Fan Speed In",6360)
                           , ("Fan Speed Out",6390)
                           ][][] Nothing
                            )
  ]

checks :: TestTree
checks = testGroup "checks to perform tests"
  [ testCase "Detect temperatures when at zero" $
    (anyTempsAreZero ([("", 0),("",55),("",5.5),("",33), ("",0),("",34)]) @?= True)
  , testCase "Don't detect zero when temperatures NOT at zero" $
    (anyTempsAreZero ([("",1),("",55),("",5.5),("",33), ("",2), ("",34)]) @?= False)
  , testCase "Don't detect temps when below threshold" $
    (anyAboveThreshold ([("",1),("",55)]) (10000.0::Rational) @?= False)
  , testCase "Detect temps when above threshold" $
    (anyAboveThreshold ([("",1),("",55),("",200)]) 100 @?= True)
  , testCase "Detect hashrates when below threshold" $
    (anyBelowThreshold) [("", 10), ("",100), ("",44)] 50 @?= True
  , testCase "Don't detect hashrates when above threshold" $
    (anyBelowThreshold) [("", 10), ("",100), ("",44)] 2 @?= False
  ]

misc :: TestTree
misc = testGroup "Other testable functionality"
  [ testCase "Replace lazy ByteString substring" $
    (replace  "12345" "" "Hello 1234512345World" @?= "Hello World")
  ]
