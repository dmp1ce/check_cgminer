import ReplyExamples
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), (@?) )

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode)

import CgminerApi (QueryApi (QueryApi), decodeReply, getStats, Stats (Stats))
import CheckCgminer (anyTempsAreZero, anyAboveThreshold, anyBelowThreshold)
import qualified Data.Text as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [json, checks]

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
                           ])
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
                           ])
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
                           ])
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
