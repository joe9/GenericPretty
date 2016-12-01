{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module TestMarket
  ( SelectionId
  , Runner(..)
  , Market(..)
  , defaultMarket
  , defaultRunner
  ) where

import qualified Data.HashMap.Strict     as HashMap
import qualified Data.IntMap.Strict      as IntMap
import           Data.String.Conversions (cs)
import           Data.Time
import           Protolude

import Text.PrettyPrint.GenericPretty

import Betfair.APING                    hiding (Runner)
import Betfair.APING.Types.MarketStatus (MarketStatus (..))
import Betfair.APING.Types.PriceSize
import Betfair.APING.Types.RunnerStatus (RunnerStatus (ACTIVE))
import Common.Types

data Runner = Runner
  { rStatus :: RunnerStatus
  , rSelectionId :: SelectionId
  , rAvailableToBack :: IntMap.IntMap PriceSize
  , rAvailableToLay :: IntMap.IntMap PriceSize
  , rTradedVolume :: HashMap.HashMap Price Size
  , rName :: Text
  , rTotalMatched :: Double
    -- below fields are derived or calculated
  , rCalculatedChangedTotalMatched :: Double
  , rCalculatedChangedTradedPriceRangeLow :: Maybe Double
  , rCalculatedChangedTradedPriceRangeHigh :: Maybe Double
  , rCalculatedBack :: Double
  , rCalculatedLay :: Double
  } deriving (Show, Read, Eq, Generic, Pretty)

defaultRunner :: Runner
defaultRunner =
  Runner
    ACTIVE
    0
    IntMap.empty
    IntMap.empty
    HashMap.empty
    ""
    0
    0
    Nothing
    Nothing
    1
    1

testRunner :: Runner
testRunner =
  Runner
    ACTIVE
    1
    (IntMap.fromList [(1, PriceSize 1.2 100), (2, PriceSize 1.1 200)])
    (IntMap.fromList [(1, PriceSize 1.3 100), (2, PriceSize 1.4 200)])
    (HashMap.fromList [(1.1, 200), (1.2, 300)])
    "testRunner"
    10000
    1000
    (Just 100)
    (Just 200)
    1
    1

data Market = Market
  { mStatus        :: MarketStatus
  , mActiveRunners :: Integer
  , mRunners       :: HashMap.HashMap SelectionId Runner
  , mName          :: MarketName
  , mEventName     :: EventName
  , mTotalMatched  :: Double
  , mAsOf          :: UTCTime
  , mMarketId      :: MarketId
  , mInPlay        :: Maybe Bool
  } deriving (Show, Read, Eq, Generic, Pretty)

-- https://github.com/haskell/time/blob/52e0f5e85ffbaab77b155d48720fb216021c8a73/lib/Data/Time/Clock/POSIX.hs#L36
unixEpoch :: UTCTime
unixEpoch = UTCTime (ModifiedJulianDay 40587) 0

showUTCTime
  :: FormatTime t
  => t -> Text
showUTCTime = cs . formatTime defaultTimeLocale rfc822DateFormat

defaultMarket :: Market
defaultMarket = Market INACTIVE 0 HashMap.empty "" "" 0 unixEpoch "" Nothing

testMarket :: Market
testMarket =
  Market
    OPEN
    1
    (HashMap.fromList [(1, testRunner)])
    "market name"
    "event name"
    100000
    unixEpoch
    "1.99999999"
    (Just True)
