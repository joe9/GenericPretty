{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GenericPretty.Tests
  ( tests
  ) where

import           GHC.Base
import           Protolude             hiding (get, put)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Data.String.Conversions      (cs)

import Text.PrettyPrint.GenericPretty

import qualified U1

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
--     [ testCase "testTversion01" testTversion01
--     , testCase "testTversion02" testTversion02
--     , testCase "testRversion01" testRversion01
--     , testCase "testRversion02" testRversion02
--     , testCase "testTattach01" testTattach01
--     , testCase "testTattach02" testTattach02
--     , testCase "testRattach01" testRattach01
--     , testCase "testRattach02" testRattach02
--     , testCase "testRerror01" testRerror01
--     , testCase "testRerror02" testRerror02
--     , testCase "testTauth01" testTauth01
--     , testCase "testTauth02" testTauth02
--     , testCase "testRauth01" testRauth01
--     , testCase "testRauth02" testRauth02
--     , testCase "testTwalk01" testTwalk01
--     , testCase "testTwalk02" testTwalk02
--     , testProperty "test Rversion" prop_Rversion
--     , testProperty "test Tversion" prop_Tversion
--     , testProperty "test Qid" prop_Qid
--     , testProperty "test Stat" prop_Stat
--     , testProperty "test Tattach" prop_Tattach
--     , testProperty "test Rattach" prop_Rattach
--     , testProperty "test Rerror" prop_Rerror
--     , testProperty "test Tauth" prop_Tauth
--     , testProperty "test Rauth" prop_Rauth
--     , testProperty "test Tflush" prop_Tflush
--     , testProperty "test Rwalk" prop_Rwalk
--     , testProperty "test Topen" prop_Topen
--     , testProperty "test Ropen" prop_Ropen
--     , testProperty "test Tcreate" prop_Tcreate
--     , testProperty "test Rcreate" prop_Rcreate
--     , testProperty "test Tread" prop_Tread
--     , testProperty "test Rread" prop_Rread
--     , testProperty "test Twrite" prop_Twrite
--     , testProperty "test Rwrite" prop_Rwrite
--     , testProperty "test Tclunk" prop_Tclunk
--     , testProperty "test Tremove" prop_Tremove
--     ]
-- it would be cool to generate the data types using Arbitrary as GAST
--  supposedly did
tests :: TestTree
tests =
  testGroup
    "NineP"
    [ -- testProperty "test U1" prop_U1
     testCase "testU1" testU1
    ]

testU1 :: Assertion
testU1 =
  U1.showAndReadEmpty U1.empty @?= Right U1.empty

-- instance Arbitrary (U1 a) where arbitrary = mempty
-- instance Arbitrary (U1 a) where arbitrary = return ()

-- prop_U1 :: U1 x -> Bool
-- prop_U1 u =
--   (fmap fst . headMay . reads . show) (displayPretty u) == Just u

-- testTversion01 :: Assertion
-- testTversion01 =
--   let result = Tversion 8192 Ver9P2000
--       resultBS = runPut (put result)
--   in runGet get resultBS @?= Right result

-- testTversion02 :: Assertion
-- testTversion02 =
--   let result = Tversion 8192 VerUnknown
--       resultBS = runPut (put result)
--   in runGet get resultBS @?= Right result

-- testRversion01 :: Assertion
-- testRversion01 =
--   let result = Rversion 8192 Ver9P2000
--       resultBS = runPut (put result)
--   in runGet get resultBS @?= Right result

-- testRversion02 :: Assertion
-- testRversion02 =
--   let result = Rversion 8192 VerUnknown
--       resultBS = runPut (put result)
--   in runGet get resultBS @?= Right result

-- testTattach01 :: Assertion
-- testTattach01 =
--   let result = Tattach 0 0 "" ""
--       resultBS = runPut (put result)
--   in runGet (get :: Get Tattach) resultBS @?= Right result

-- testTattach02 :: Assertion
-- testTattach02 =
--   let result = Tattach 0 0 "" ""
--       resultBS = runPut (put result)
--   in runGet (get :: Get Tattach) resultBS @?= Right result

-- testRattach01 :: Assertion
-- testRattach01 =
--   let result = Rattach (Qid [] 0 0)
--       resultBS = runPut (put result)
--   in runGet (get :: Get Rattach) resultBS @?= Right result

-- testRattach02 :: Assertion
-- testRattach02 =
--   let result = Rattach (Qid [] 0 0)
--       resultBS = runPut (put result)
--   in runGet (get :: Get Rattach) resultBS @?= Right result

-- testRerror01 :: Assertion
-- testRerror01 =
--   let result = Rerror "test"
--       resultBS = runPut (put result)
--   in runGet (get :: Get Rerror) resultBS @?= Right result

-- testRerror02 :: Assertion
-- testRerror02 =
--   let result = Rerror "test"
--       resultBS = runPut (put result)
--   in runGet (get :: Get Rerror) resultBS @?= Right result

-- testTauth01 :: Assertion
-- testTauth01 =
--   let result = Tauth 0 "" ""
--       resultBS = runPut (put result)
--   in runGet (get :: Get Tauth) resultBS @?= Right result

-- testTauth02 :: Assertion
-- testTauth02 =
--   let result = Tauth 0 "" ""
--       resultBS = runPut (put result)
--   in runGet (get :: Get Tauth) resultBS @?= Right result

-- testRauth01 :: Assertion
-- testRauth01 =
--   let result = Rauth (Qid [] 0 0)
--       resultBS = runPut (put result)
--   in runGet (get :: Get Rauth) resultBS @?= Right result

-- testRauth02 :: Assertion
-- testRauth02 =
--   let result = Rauth (Qid [] 0 0)
--       resultBS = runPut (put result)
--   in runGet (get :: Get Rauth) resultBS @?= Right result

-- testTwalk01 :: Assertion
-- testTwalk01 =
--   let result = Twalk 0 0 [""]
--       resultBS = runPut (put result)
--   in runGet (get :: Get Twalk) resultBS @?= Right result

-- testTwalk02 :: Assertion
-- testTwalk02 =
--   let result = Twalk 0 0 [""]
--       resultBS = runPut (put result)
--   in runGet (get :: Get Twalk) resultBS @?= Right result

-- -- testRwalk01 :: Assertion
-- -- testRwalk01 =
-- --   let result = Rwalk [(Qid [] 0 0)]
-- --       resultBS = runPut (put result)
-- --   in runGet ( get :: Get Rwalk) resultBS @?= Right result
-- -- testRwalk02 :: Assertion
-- -- testRwalk02 =
-- --   let result = Rwalk [(Qid [] 0 0)]
-- --       resultBS = runPut (put result)
-- --   in runGet ( get :: Get Rwalk) resultBS @?= Right result
-- prop_Rversion :: Rversion -> Bool
-- prop_Rversion result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tversion :: Tversion -> Bool
-- prop_Tversion result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tattach :: Tattach -> Bool
-- prop_Tattach result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Qid :: Qid -> Bool
-- prop_Qid result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- --   let resultBS = runPut (put (traceShow result result))
-- --       output = runGet get (traceShow (showByteStringInHex resultBS) resultBS)
-- --   in  traceShow output output == Right result
-- prop_Stat :: Stat -> Bool
-- prop_Stat result =
--   let resultBS = runPut (put result)
--       output = runGet get resultBS
--       -- the below line hangs
--       --  toutput = traceShow output output
--   in output == Right result

-- --   let resultBS = runPut (put (traceShow result result))
-- --       output = runGet get (traceShow (showByteStringInHex resultBS) resultBS) :: Either String Stat
-- --   -- the below line hangs
-- --   --  toutput = traceShow output output
-- --   in  output == Right result
-- prop_Rattach :: Rattach -> Bool
-- prop_Rattach result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- --   let resultBS = runPut (put (traceShow result result))
-- --   in runGet get (traceShow resultBS resultBS) == Right result
-- prop_Rerror :: Rerror -> Bool
-- prop_Rerror result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tauth :: Tauth -> Bool
-- prop_Tauth result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Rauth :: Rauth -> Bool
-- prop_Rauth result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tflush :: Tflush -> Bool
-- prop_Tflush result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- -- prop_Rflush :: Rflush -> Bool
-- -- prop_Rflush result =
-- --   let resultBS = runPut (put result)
-- --   in runGet get resultBS == Right result
-- prop_Twalk :: Twalk -> Bool
-- prop_Twalk result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Rwalk :: Rwalk -> Bool
-- prop_Rwalk result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Topen :: Topen -> Bool
-- prop_Topen result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Ropen :: Ropen -> Bool
-- prop_Ropen result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tcreate :: Tcreate -> Bool
-- prop_Tcreate result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Rcreate :: Rcreate -> Bool
-- prop_Rcreate result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tread :: Tread -> Bool
-- prop_Tread result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Rread :: Rread -> Bool
-- prop_Rread result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Twrite :: Twrite -> Bool
-- prop_Twrite result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Rwrite :: Rwrite -> Bool
-- prop_Rwrite result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Tclunk :: Tclunk -> Bool
-- prop_Tclunk result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- -- prop_Rclunk :: Rclunk -> Bool
-- -- prop_Rclunk result =
-- --   let resultBS = runPut (put result)
-- --   in runGet get resultBS == Right result
-- prop_Tremove :: Tremove -> Bool
-- prop_Tremove result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- -- prop_Rremove :: Rremove -> Bool
-- -- prop_Rremove result =
-- --   let resultBS = runPut (put result)
-- --   in runGet get resultBS == Right result
-- prop_Tstat :: Tstat -> Bool
-- prop_Tstat result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Rstat :: Rstat -> Bool
-- prop_Rstat result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result

-- prop_Twstat :: Twstat -> Bool
-- prop_Twstat result =
--   let resultBS = runPut (put result)
--   in runGet get resultBS == Right result
-- -- prop_Rwstat :: Rwstat -> Bool
-- -- prop_Rwstat result =
-- --   let resultBS = runPut (put result)
-- --   in runGet get resultBS == Right result
