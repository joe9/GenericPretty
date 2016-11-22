{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sum where

import Data.String.Conversions (cs)
import GHC.Base
import GHC.Generics
import Protolude               hiding (empty, Sum)

import Text.PrettyPrint.GenericPretty

data Sum = Cat | Dog | Rat | Elephant
  deriving (Eq, Read, Show, Generic, Pretty)

sample :: Sum
sample = Elephant

showAndRead :: Sum -> Either GHC.Base.String Sum
showAndRead = readEither . cs . displayPretty
