{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module GenericPretty.Tests
  ( tests
  ) where

import Protolude        hiding (Product, Sum, empty)
import Test.Tasty
import Test.Tasty.HUnit

import qualified K1
import qualified Product
import qualified Sum
import qualified TestData
import qualified TestMarket
import qualified U1

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
-- it would be cool to generate the data types using Arbitrary as GAST
--  supposedly did
-- testProperty "test U1" prop_U1
tests :: TestTree
tests =
  testGroup
    "GenericPretty"
    [ testCase "testU1" testU1
    , testCase "testSum" testSum
    , testCase "testProduct" testProduct
    , testCase "testK1" testK1
    , testCase "testDataTree1" testDataTree1
    , testCase "testDataTree2" testDataTree2
    , testCase "testDataTree3" testDataTree3
    , testCase "testDataTree4" testDataTree4
    , testCase "testDataAnimal1" testDataAnimal1
    , testCase "testMarket" testMarket
    ]

testU1 :: Assertion
testU1 = U1.showAndRead U1.sample @?= Right U1.sample

testSum :: Assertion
testSum = Sum.showAndRead Sum.sample @?= Right Sum.sample

testProduct :: Assertion
testProduct = Product.showAndRead Product.sample @?= Right Product.sample

testK1 :: Assertion
testK1 = K1.showAndRead K1.sample @?= Right K1.sample

testDataTree1 :: Assertion
testDataTree1 = TestData.showAndReadTree TestData.tree1 @?= Right TestData.tree1

testDataTree2 :: Assertion
testDataTree2 = TestData.showAndReadTree TestData.tree2 @?= Right TestData.tree2

testDataTree3 :: Assertion
testDataTree3 = TestData.showAndReadTree TestData.tree3 @?= Right TestData.tree3

testDataTree4 :: Assertion
testDataTree4 =
  TestData.showAndReadTree1 TestData.tree4 @?= Right TestData.tree4

testDataAnimal1 :: Assertion
testDataAnimal1 =
  TestData.showAndReadAnimal1 TestData.animal1 @?= Right TestData.animal1

testMarket :: Assertion
testMarket =
  TestData.showAndReadMarket TestMarket.testMarket @?= Right TestMarket.testMarket
