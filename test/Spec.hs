import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), (@?))

import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode)

import CgminerApi (QueryApi (QueryApi), decodeReply, getTemps, Tempuratures (Tempuratures))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [json]

exampleSummaryCommand :: ByteString
exampleSummaryCommand = "{\"command\":\"summary\",\"parameter\":\"0\"}"

json :: TestTree
json = testGroup "Unit tests"

  [ testCase "encode QueryApi should match example command text" $
    (encode $ QueryApi "summary" "0") @?= exampleSummaryCommand
  , testCase "Successfully decode example reply" $
    ((decodeReply exampleReply) /= Nothing) @? "exampleReply could not be decoded"
  , testCase "Can get temps" $
      let Just x = decodeReply exampleReply
      in (getTemps x) @?= (Right $ Tempuratures (59, 75) (54, 71) (55, 74))
  ]

-- Example command used to get summary from cgminer

-- "{\"command\":\"summary\",\"parameter\":\"0\"}"

-- Example from "stats" command

exampleReply :: ByteString
exampleReply = "{\"STATUS\":[{\"STATUS\":\"S\",\"When\":1551730659,\"Code\":70,\"Msg\":\"BMMiner stats\",\"Description\":\"bmminer bOS_am1-s9-20190124-0_d3cc7c87\"}],\"STATS\":[{\"BMMiner\":\"bOS_am1-s9-20190124-0_d3cc7c87\",\"Miner\":\"bOS_am1-s9-20190124-0_d3cc7c87\",\"CompileTime\":\"\",\"Type\":\"braiins-am1-s9\"},{\"STATS\":1,\"ID\":\"BC50\",\"Elapsed\":1738925,\"Calls\":0,\"Wait\":0.000000,\"Max\":0.000000,\"Min\":99999999.000000,\"GHS 5s\":\"13754.03\",\"GHS av\":13767.86,\"miner_count\":3,\"frequency\":\"656\",\"fan_num\":2,\"fan1\":0,\"fan2\":0,\"fan3\":0,\"fan4\":0,\"fan5\":5040,\"fan6\":3600,\"fan7\":0,\"fan8\":0,\"voltage1\":0.000000,\"voltage2\":0.000000,\"voltage3\":0.000000,\"voltage4\":0.000000,\"voltage5\":0.000000,\"voltage6\":8.900000,\"voltage7\":8.700000,\"voltage8\":8.900000,\"voltage9\":0.000000,\"voltage10\":0.000000,\"voltage11\":0.000000,\"voltage12\":0.000000,\"voltage13\":0.000000,\"voltage14\":0.000000,\"voltage15\":0.000000,\"voltage16\":0.000000,\"temp_num\":3,\"temp1\":0.00,\"temp2\":0.00,\"temp3\":0.00,\"temp4\":0.00,\"temp5\":0.00,\"temp6\":59.00,\"temp7\":54.00,\"temp8\":55.00,\"temp9\":0.00,\"temp10\":0.00,\"temp11\":0.00,\"temp12\":0.00,\"temp13\":0.00,\"temp14\":0.00,\"temp15\":0.00,\"temp16\":0.00,\"temp2_1\":0.00,\"temp2_2\":0.00,\"temp2_3\":0.00,\"temp2_4\":0.00,\"temp2_5\":0.00,\"temp2_6\":75.00,\"temp2_7\":71.00,\"temp2_8\":74.00,\"temp2_9\":0.00,\"temp2_10\":0.00,\"temp2_11\":0.00,\"temp2_12\":0.00,\"temp2_13\":0.00,\"temp2_14\":0.00,\"temp2_15\":0.00,\"temp2_16\":0.00,\"temp3_1\":0,\"temp3_2\":0,\"temp3_3\":0,\"temp3_4\":0,\"temp3_5\":0,\"temp3_6\":0,\"temp3_7\":0,\"temp3_8\":0,\"temp3_9\":0,\"temp3_10\":0,\"temp3_11\":0,\"temp3_12\":0,\"temp3_13\":0,\"temp3_14\":0,\"temp3_15\":0,\"temp3_16\":0,\"freq_desc6\":\"factory set frequency 631 MHz\",\"freq_desc7\":\"factory set frequency 656 MHz\",\"freq_desc8\":\"factory set frequency 631 MHz\",\"freq_avg1\":0.00,\"freq_avg2\":0.00,\"freq_avg3\":0.00,\"freq_avg4\":0.00,\"freq_avg5\":0.00,\"freq_avg6\":631.00,\"freq_avg7\":656.00,\"freq_avg8\":631.00,\"freq_avg9\":0.00,\"freq_avg10\":0.00,\"freq_avg11\":0.00,\"freq_avg12\":0.00,\"freq_avg13\":0.00,\"freq_avg14\":0.00,\"freq_avg15\":0.00,\"freq_avg16\":0.00,\"total_rateideal\":13755.69,\"total_freqavg\":639.33,\"total_acn\":189,\"total_rate\":13754.01,\"chain_rateideal1\":0.00,\"chain_rateideal2\":0.00,\"chain_rateideal3\":0.00,\"chain_rateideal4\":0.00,\"chain_rateideal5\":0.00,\"chain_rateideal6\":4525.53,\"chain_rateideal7\":4699.58,\"chain_rateideal8\":4530.58,\"chain_rateideal9\":0.00,\"chain_rateideal10\":0.00,\"chain_rateideal11\":0.00,\"chain_rateideal12\":0.00,\"chain_rateideal13\":0.00,\"chain_rateideal14\":0.00,\"chain_rateideal15\":0.00,\"chain_rateideal16\":0.00,\"temp_max\":0,\"Device Hardware%\":0.0001,\"no_matching_work\":4428,\"chain_acn1\":0,\"chain_acn2\":0,\"chain_acn3\":0,\"chain_acn4\":0,\"chain_acn5\":0,\"chain_acn6\":63,\"chain_acn7\":63,\"chain_acn8\":63,\"chain_acn9\":0,\"chain_acn10\":0,\"chain_acn11\":0,\"chain_acn12\":0,\"chain_acn13\":0,\"chain_acn14\":0,\"chain_acn15\":0,\"chain_acn16\":0,\"chain_cores6\":7172,\"chain_cores7\":7164,\"chain_cores8\":7180,\"chain_acs1\":\"\",\"chain_acs2\":\"\",\"chain_acs3\":\"\",\"chain_acs4\":\"\",\"chain_acs5\":\"\",\"chain_acs6\":\" oooooooo oooooooo oooooooo oooooooo oooooooo oooooooo oooooooo ooooooo\",\"chain_acs7\":\" oooooooo oooooooo oooooooo oooooooo oooooooo oooooooo oooooooo ooooooo\",\"chain_acs8\":\" oooooooo oooooooo oooooooo oooooooo oooooooo oooooooo oooooooo ooooooo\",\"chain_acs9\":\"\",\"chain_acs10\":\"\",\"chain_acs11\":\"\",\"chain_acs12\":\"\",\"chain_acs13\":\"\",\"chain_acs14\":\"\",\"chain_acs15\":\"\",\"chain_acs16\":\"\",\"chain_hw1\":0,\"chain_hw2\":0,\"chain_hw3\":0,\"chain_hw4\":0,\"chain_hw5\":0,\"chain_hw6\":844,\"chain_hw7\":1850,\"chain_hw8\":1732,\"chain_hw9\":0,\"chain_hw10\":0,\"chain_hw11\":0,\"chain_hw12\":0,\"chain_hw13\":0,\"chain_hw14\":0,\"chain_hw15\":0,\"chain_hw16\":0,\"chain_hwrate1\":0.000000,\"chain_hwrate2\":0.000000,\"chain_hwrate3\":0.000000,\"chain_hwrate4\":0.000000,\"chain_hwrate5\":0.000000,\"chain_hwrate6\":0.854701,\"chain_hwrate7\":5.162510,\"chain_hwrate8\":2.931596,\"chain_hwrate9\":0.000000,\"chain_hwrate10\":0.000000,\"chain_hwrate11\":0.000000,\"chain_hwrate12\":0.000000,\"chain_hwrate13\":0.000000,\"chain_hwrate14\":0.000000,\"chain_hwrate15\":0.000000,\"chain_hwrate16\":0.000000,\"chain_rate1\":\"\",\"chain_rate2\":\"\",\"chain_rate3\":\"\",\"chain_rate4\":\"\",\"chain_rate5\":\"\",\"chain_rate6\":\"4524.28\",\"chain_rate7\":\"4679.16\",\"chain_rate8\":\"4550.58\",\"chain_rate9\":\"\",\"chain_rate10\":\"\",\"chain_rate11\":\"\",\"chain_rate12\":\"\",\"chain_rate13\":\"\",\"chain_rate14\":\"\",\"chain_rate15\":\"\",\"chain_rate16\":\"\",\"chain_xtime6\":\"{X52=1,X54=1}\",\"chain_xtime7\":\"{X9=1,X39=1}\",\"chain_xtime8\":\"{X34=1}\",\"chain_offside_6\":\"0\",\"chain_offside_7\":\"0\",\"chain_offside_8\":\"0\",\"chain_opencore_6\":\"0\",\"chain_opencore_7\":\"0\",\"chain_opencore_8\":\"0\",\"miner_version\":\"26.0.1.3\"}],\"id\":1}\NUL"

-- Example output using netcat from cgminer api

--echo '{"command":"summary","parameter":"0"}' | nc -v 10.0.0.55 4028
--10.0.0.55 4028 (dtserver-port) open
--{"STATUS":[{"STATUS":"S","When":1551457715,"Code":11,"Msg":"Summary","Description":"bmminer bOS_am1-s9-20190124-0_d3cc7c87"}],"SUMMARY":[{"Elapsed":1465981,"GHS 5s":"13759.68","GHS av":13769.04,"Hashrate1m":14179.421609,"Hashrate15m":13610.051011,"Hashrate24h":13742.172463,"Found Blocks":0,"Getworks":52564,"Accepted":441965,"Rejected":1777,"Hardware Errors":3713,"Utility":18.09,"Discarded":787965,"Stale":103,"Get Failures":5,"Local Work":74285634,"Remote Failures":5,"Network Blocks":2471,"Total MH":20185140718075.0000,"Work Utility":192927.57,"Difficulty Accepted":4695905785.00000000,"Difficulty Rejected":17896825.00000000,"Difficulty Stale":0.00000000,"Best Share":20492871957,"Device Hardware%":0.0001,"Device Rejected%":0.3797,"Pool Rejected%":0.3797,"Pool Stale%":0.0000,"Last getwork":1551457714}],"id":1}
