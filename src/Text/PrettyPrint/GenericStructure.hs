{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Text.PrettyPrint.GenericStructure
  ( Structure(..)
  , Generic
  , displayStructure
  , structure
  ) where

import qualified Data.HashMap.Strict
import qualified Data.IntMap
import           Data.IxSet.Typed        (Indexable)
import qualified Data.IxSet.Typed
import qualified Data.Map
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           Data.Text.Lazy          (Text, append, fromStrict,
                                          intercalate, singleton,
                                          unlines, unwords)
import           Data.Time
import           GHC.Generics
import           Protolude               hiding (Text, bool,
                                          intercalate, intersperse)

-- | The class 'Structure' is the equivalent of 'Prelude.Show'
--
-- It provides conversion of values to structure printable Structure.Text's.
--
class Structure a where
  structure :: a -> Text
  default structure :: (Generic a, GStructure (Rep a)) =>
    a -> Text
  structure x =
    intercalate
      "\n"
      [ "structure begin"
      , intercalate "\n" ((gstructure . from) x)
      , "structure end"
      ]

--   structure x = case (gstructure . from) x of
--                (o:[]) -> o
--                os -> PP.list os
--'GStructure' is a helper class used to output the Sum-of-Products type, since it has kind *->*,
-- so can't be an instance of 'Structure'
class GStructure f
      -- |'gstructure' is the (*->*) kind equivalent of 'docPrec'
                                                                  where
  gstructure :: f x -> [Text]

-- if empty, output nothing, this is a null constructor
instance GStructure U1 where
  gstructure _ = ["M1 U Begin" <> "M1 U End"]

--                                 , append "datatypeName: " ((show . datatypeName) a)
--                                 , append "moduleName: " ((cs . moduleName) d)
--                                 , append "packageName: " ((cs .  packageName) d)
--                                 , append "isNewtype: " ((show . isNewtype) d)
instance (GStructure f) =>
         GStructure (M1 D c f) where
  gstructure (M1 a) =
    [ intercalate
        "\n"
        ["M1 D Begin", intercalate "\n" (gstructure a), "M1 D End"]
    ]

instance (GStructure f, Selector c) =>
         GStructure (M1 S c f) where
  gstructure s@(M1 a) =
    [ intercalate
        "\n"
        [ "M1 S Begin"
        , "selName: " <> (cs . selName) s
        , intercalate "\n" (gstructure a)
        , "M1 S End"
        ]
    ]

-- constructor
-- here the real type and parens flag is set and propagated forward via t and n, the precedence factor is updated
instance (GStructure f, Constructor c) =>
         GStructure (M1 C c f) where
  gstructure c@(M1 a) =
    [ intercalate
        "\n"
        [ "M1 C Begin"
        , "conName: " <> (cs . conName) c
        , "conFixity: " <> (show . conFixity) c
        , "conIsRecord: " <> (show . conIsRecord) c
        , intercalate "\n" (gstructure a)
        , "M1 C End"
        ]
    ]

-- ignore tagging, call docPrec since these are concrete types
instance (Structure f) =>
         GStructure (K1 t f) where
  gstructure (K1 a) = [intercalate "\n" ["M1 K Begin", structure a, "M1 K End"]]

-- just continue to the corresponding side of the OR
instance (GStructure a, GStructure b) =>
         GStructure (a :+: b) where
  gstructure (L1 x) =
    [ intercalate
        "\n"
        ["M1 :+: L1 Begin", intercalate "\n" (gstructure x), "M1 :+: L1 End"]
    ]
  gstructure (R1 x) =
    [ intercalate
        "\n"
        ["M1 :*: R1 Begin", intercalate "\n" (gstructure x), "M1 :*: R1 End"]
    ]

-- output both sides of the product, possible separated by a comma or an infix operator
instance (GStructure a, GStructure b) =>
         GStructure (a :*: b) where
  gstructure (x :*: y) =
    [ intercalate
        "\n"
        [ "M1 :*: Begin"
        , intercalate "\n" (gstructure x)
        , ":*:"
        , intercalate "\n" (gstructure y)
        , "M1 :*: End"
        ]
    ]

instance Structure Char where
  structure = singleton

instance Structure Text where
  structure = identity

instance Structure T.Text where
  structure = fromStrict

instance Structure Int where
  structure = show

instance Structure Integer where
  structure = show

instance Structure Float where
  structure = show

instance Structure Double where
  structure = show

instance Structure Rational where
  structure = show

instance Structure Bool where
  structure = show

instance Structure ByteString where
  structure = cs

instance Structure a =>
         Structure [a] where
  structure = show . fmap structure

instance Structure a =>
         Structure (Maybe a) where
  structure Nothing  = "Nothing"
  structure (Just x) = "Just " <> structure x

instance (Structure a, Structure b) =>
         Structure (Either a b) where
  structure (Left x)  = "Left " <> structure x
  structure (Right y) = "Right " <> structure y

instance (Structure a, Structure b) =>
         Structure (a, b) where
  structure (a, b) =
    "(" <> show (structure a) <> " , " <> show (structure b) <> ")"

instance (Structure a, Structure b) =>
         Structure (Data.Map.Map a b) where
  structure v = "fromList " <> structure v

instance Structure UTCTime where
  structure = cs . formatTime defaultTimeLocale rfc822DateFormat

instance (Show a, Indexable ixs a) =>
         Structure (Data.IxSet.Typed.IxSet ixs a) where
  structure = show

instance Structure Word where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Word8 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Word16 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Word32 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Word64 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Int8 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Int16 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Int32 where
  structure = (structure :: Integer -> Text) . fromIntegral

instance Structure Int64 where
  structure = (structure :: Integer -> Text) . fromIntegral

displayStructure
  :: Structure a
  => a -> T.Text
displayStructure = toStrict . structure
