{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module U1 where

import Data.String.Conversions (cs)
import GHC.Base
import GHC.Generics
import Protolude               hiding (empty, get, put)

import Text.PrettyPrint.GenericPretty

data Empty =
  Empty
  deriving (Eq, Read, Show, Generic, Pretty)

sample :: Empty
sample = Empty

showAndRead :: Empty -> Either GHC.Base.String Empty
showAndRead = readEither . cs . displayPretty
