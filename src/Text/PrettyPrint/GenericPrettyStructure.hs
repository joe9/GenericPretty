{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Text.PrettyPrint.GenericPrettyStructure
  ( PrettyStructure(..)
  , Generic
  , displayPrettyStructure
  , prettyStructure
  ) where

import qualified Data.HashMap.Strict
import qualified Data.IntMap
import           Data.IxSet.Typed             (Indexable)
import qualified Data.IxSet.Typed
import qualified Data.Map
import           Data.String.Conversions      (cs)
import qualified Data.Text                    as T
import           Data.Text.Lazy               (Text, append,
                                               fromStrict,
                                               intercalate, singleton,
                                               unlines, unwords)
import           Data.Time
import           GHC.Generics
import           Protolude                    hiding (Text, bool,
                                               intercalate,
                                               intersperse)
import           Text.PrettyPrint.Leijen.Text hiding (Pretty (..),
                                               (<$>), (<>))
import qualified Text.PrettyPrint.Leijen.Text as PP

-- | The class 'PrettyStructure' is the equivalent of 'Prelude.Show'
--
-- It provides conversion of values to prettyStructure printable PrettyStructure.Text's.
--
class PrettyStructure a where
  prettyStructure :: a -> Doc
  default prettyStructure :: (Generic a, GPrettyStructure (Rep a)) =>
    a -> Doc
  prettyStructure x =
    string "prettyStructure begin" PP.<$>
    nest 3 (fillSep ((gprettyStructure . from) x)) PP.<$>
    string "prettyStructure end"

--   prettyStructure x = case (gprettyStructure . from) x of
--                (o:[]) -> o
--                os -> PP.list os
--'GPrettyStructure' is a helper class used to output the Sum-of-Products type, since it has kind *->*,
-- so can't be an instance of 'PrettyStructure'
-- |'gprettyStructure' is the (*->*) kind equivalent of 'docPrec'
class GPrettyStructure f where
  gprettyStructure :: f x -> [Doc]

-- if empty, output nothing, this is a null constructor
instance GPrettyStructure U1 where
  gprettyStructure _ = [string "M1 U Begin" PP.<> string "M1 U End"]

--                                 , append "datatypeName: " ((show . datatypeName) a)
--                                 , append "moduleName: " ((cs . moduleName) d)
--                                 , append "packageName: " ((cs .  packageName) d)
--                                 , append "isNewtype: " ((show . isNewtype) d)
-- ignore datatype meta-information
instance (GPrettyStructure f) =>
         GPrettyStructure (M1 D c f) where
  gprettyStructure (M1 a) =
    [ (string "M1 D Begin") PP.<$>
      ((nest 3 . fillSep) (gprettyStructure a)) PP.<$> (string "M1 D End")
    ]

instance (GPrettyStructure f, Selector c) =>
         GPrettyStructure (M1 S c f) where
  gprettyStructure s@(M1 a) =
    [string "M1 S Begin" PP.<$>
        (nest 3 . fillSep) [ string ("selName: " <> (cs . selName) s)
                            , fillSep (gprettyStructure a)]
        PP.<$> string "M1 S End"
    ]

-- constructor
-- here the real type and parens flag is set and propagated forward via t and n, the precedence factor is updated
instance (GPrettyStructure f, Constructor c) =>
         GPrettyStructure (M1 C c f) where
  gprettyStructure c@(M1 a) =
    [  string "M1 C Begin" PP.<$>
        (nest 3 . fillSep)[
                            string ("conName: " <> (cs . conName) c)
                            , string ("conFixity: " <> (show . conFixity) c)
                            , string ("conIsRecord: " <> (show . conIsRecord) c)
                            , fillSep (gprettyStructure a)
                         ]
        PP.<$> string "M1 C End"
    ]

-- ignore tagging, call docPrec since these are concrete types
instance (PrettyStructure f) =>
         GPrettyStructure (K1 t f) where
  gprettyStructure (K1 a) =
--     [string "M1 K Begin" <+> (nest 3 . prettyStructure) a PP.<+> string "M1 K End"]
    [string "M1 K Begin" PP.<$> (nest 3 . prettyStructure) a PP.<$> string "M1 K End"]

-- just continue to the corresponding side of the OR
instance (GPrettyStructure a, GPrettyStructure b) =>
         GPrettyStructure (a :+: b) where
  gprettyStructure (L1 x) =
    [ string "M1 :+: L1 Begin" PP.<$>
           (nest 3 . fillSep) (gprettyStructure x)
           PP.<$> string "M1 :+: L1 End"
    ]
--     [ vcat [ string "M1 :+: L1 Begin"
--            , (nest 1 . nest 3 . fillSep) (gprettyStructure x)
--            , string "\nM1 :+: L1 End"]
--     ]
--     [ string "M1 :+: L1 Begin" <+>
--         (nest 3 . fillSep) (gprettyStructure x)
--         PP.<> string "\nM1 :+: L1 End"
--     ]
  gprettyStructure (R1 x) =
    [ string "M1 :*: R1 Begin" PP.<$>
        (nest 3 . fillSep) (gprettyStructure x)
        PP.<$> string "M1 :*: R1 End"
    ]

-- output both sides of the product, possible separated by a comma or an infix operator
instance (GPrettyStructure a, GPrettyStructure b) =>
         GPrettyStructure (a :*: b) where
  gprettyStructure (x :*: y) =
    [ sep
        [ string "M1 :*: Begin"
        , (nest 1 . sep) (gprettyStructure x)
        , string ":*:"
        , (nest 1 . sep) (gprettyStructure y)
        , string "M1 :*: End"
        ]
    ]

instance PrettyStructure Char where
  prettyStructure = char

instance PrettyStructure Text where
  prettyStructure = string

instance PrettyStructure T.Text where
  prettyStructure = string . fromStrict

instance PrettyStructure Int where
  prettyStructure = int

instance PrettyStructure Integer where
  prettyStructure = integer

instance PrettyStructure Float where
  prettyStructure = float

instance PrettyStructure Double where
  prettyStructure = double

instance PrettyStructure Rational where
  prettyStructure = rational

instance PrettyStructure Bool where
  prettyStructure = bool

instance PrettyStructure ByteString where
  prettyStructure = string . cs

instance PrettyStructure a =>
         PrettyStructure [a] where
  prettyStructure = PP.list . fmap prettyStructure

instance PrettyStructure a =>
         PrettyStructure (Maybe a) where
  prettyStructure Nothing  = "Nothing"
  prettyStructure (Just x) = sep [string "Just", prettyStructure x]

instance (PrettyStructure a, PrettyStructure b) =>
         PrettyStructure (Either a b) where
  prettyStructure (Left x) = sep [string "Left", prettyStructure x]
  prettyStructure (Right y) = sep [string "Right", prettyStructure y]

instance (PrettyStructure a, PrettyStructure b) =>
         PrettyStructure (a, b) where
  prettyStructure (a, b) = tupled [prettyStructure a, prettyStructure b]

instance (PrettyStructure a, PrettyStructure b) =>
         PrettyStructure (Data.Map.Map a b) where
  prettyStructure v =
    sep [string "fromList ", (prettyStructure . Data.Map.toList) v]

instance PrettyStructure UTCTime where
  prettyStructure = text . cs . formatTime defaultTimeLocale rfc822DateFormat

instance (Show a, Indexable ixs a) =>
         PrettyStructure (Data.IxSet.Typed.IxSet ixs a) where
  prettyStructure = text . show

instance PrettyStructure Word where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Word8 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Word16 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Word32 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Word64 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Int8 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Int16 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Int32 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

instance PrettyStructure Int64 where
  prettyStructure = (prettyStructure :: Integer -> Doc) . fromIntegral

displayPrettyStructureL
  :: PrettyStructure a
  => a -> Text
displayPrettyStructureL = PP.displayT . PP.renderPretty 1.0 70 . prettyStructure

displayPrettyStructure
  :: PrettyStructure a
  => a -> T.Text
displayPrettyStructure = toStrict . displayPrettyStructureL
