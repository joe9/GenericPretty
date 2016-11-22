{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import Test.Tasty (defaultMain, testGroup)

import qualified GenericPretty.Tests

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
main :: IO ()
main = defaultMain $ testGroup "Tests" [GenericPretty.Tests.tests]
