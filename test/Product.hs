{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Product where

import Data.String.Conversions (cs)
import GHC.Base
import GHC.Generics
import Protolude               hiding (Product)

import Text.PrettyPrint.GenericPretty

data Product = Product Int Integer Char Text ByteString
  deriving (Eq, Read, Show, Generic, Pretty)

sample :: Product
sample = Product 1 10 'C' "sample text" "sample bytestring"

showAndRead :: Product -> Either GHC.Base.String Product
showAndRead = readEither . cs . displayPretty
