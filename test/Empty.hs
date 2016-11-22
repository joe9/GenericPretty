{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Empty where

import Data.String.Conversions (cs)
import GHC.Base
import GHC.Generics
import Protolude               hiding (empty, get, put)

import Text.PrettyPrint.GenericPretty

data Empty =
  Empty
  deriving (Eq, Read, Show, Generic, Pretty)

empty :: Empty
empty = Empty

showAndReadEmpty :: Empty -> Either GHC.Base.String Empty
showAndReadEmpty = readEither . cs . displayPretty
