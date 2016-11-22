{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module K1 where

import Data.String.Conversions (cs)
import GHC.Base
import GHC.Generics
import Protolude               hiding (K1)

import Text.PrettyPrint.GenericPretty

data Sample = Sample ByteString
  deriving (Eq, Read, Show, Generic, Pretty)

sample :: Sample
sample = Sample "sample bytestring"

showAndRead :: Sample -> Either GHC.Base.String Sample
showAndRead = readEither . cs . displayPretty
