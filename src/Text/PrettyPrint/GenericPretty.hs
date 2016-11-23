{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-|
  GenericPretty is a Haskell library that supports automatic
  derivation of pretty printing functions on user defined data
  types.

        The output provided is a pretty printed version of that provided by
  'Prelude.show'.  That is, rendering the document provided by this pretty
  printer yields an output identical to that of 'Prelude.show', except
  for extra whitespace.

        For examples of usage please see the README file included in the package.

  For more information see the HackageDB project page: <http://hackage.haskell.org/package/GenericPretty>
-}
module Text.PrettyPrint.GenericPretty
  ( Pretty(..)
  , Generic
  , displayPrettyL
  , displayPretty
  , displayPrettyLenL
  , displayPrettyLen
  , displayPrettyPrefixL
  , displayPrettyPrefix
  ) where

import qualified Data.HashMap.Strict
import qualified Data.IntMap
import           Data.IxSet.Typed             (Indexable)
import qualified Data.IxSet.Typed
import qualified Data.Map
import           Data.String.Conversions      (cs)
import qualified Data.Text                    as T
import           Data.Text.Lazy               (Text, fromStrict)
import           Data.Time
import           GHC.Generics
import           Protolude                    hiding (Text, bool,
                                               (<$>), (<>))
import           Text.PrettyPrint.Leijen.Text hiding (Pretty (..),
                                               (<>))
import qualified Text.PrettyPrint.Leijen.Text as PP

-- | The class 'Pretty' is the equivalent of 'Prelude.Show'
--
-- It provides conversion of values to pretty printable Pretty.Doc's.
--
class Pretty a where
  pretty :: a -> Doc
  default pretty :: (Generic a, GPretty (Rep a)) =>
    a -> Doc
  pretty x =
    case (gpretty . from) x of
      (o:[]) -> o
      os     -> PP.list os

--'GPretty' is a helper class used to output the Sum-of-Products type, since it has kind *->*,
-- so can't be an instance of 'Pretty'
class GPretty f
      -- |'gpretty' is the (*->*) kind equivalent of 'docPrec'
                                                               where
  gpretty :: f x -> [Doc]

-- if empty, output nothing, this is a null constructor
instance GPretty U1 where
  gpretty _ = []

-- ignore datatype meta-information
instance (GPretty f) =>
         GPretty (M1 D c f) where
  gpretty (M1 a) = gpretty a

instance (GPretty f, Selector c) =>
         GPretty (M1 S c f) where
  gpretty s@(M1 a)
    | selector == "" = gpretty a
    | otherwise =
      if null components
        then []
        else [nest prefixLength ( string (cs selector) <+> char '=' <+> cat components)]
    where
      selector = selName s
      components = gpretty a
      prefixLength = length selector + 3

--         | otherwise = Just PP.empty
-- constructor
-- here the real type and parens flag is set and propagated forward via t and n, the precedence factor is updated
instance (GPretty f, Constructor c) =>
         GPretty (M1 C c f) where
  gpretty c@(M1 a)
    | null components = [(string . cs . conName) c]
    | conIsRecord c =
      [ (string . cs . conName) c <+>
        (braces . align . fillSep . punctuate comma) components
      ]
    | otherwise =
      [parens ((string . cs . conName) c <+> (align . sep) components)]
    where
      components = gpretty a

-- ignore tagging, call docPrec since these are concrete types
instance (Pretty f) =>
         GPretty (K1 t f) where
  gpretty (K1 a) = [(pretty a)]

-- output both sides of the product, possible separated by a comma or an infix operator
instance (GPretty a, GPretty b) =>
         GPretty (a :*: b) where
  gpretty (x :*: y) = xs ++ ys
    where
      xs = gpretty x
      ys = gpretty y

-- just continue to the corresponding side of the OR
instance (GPretty a, GPretty b) =>
         GPretty (a :+: b) where
  gpretty (L1 x) = gpretty x
  gpretty (R1 x) = gpretty x

instance Pretty Char where
  pretty = squotes . char

instance Pretty Text where
  pretty = dquotes . string

instance Pretty T.Text where
  pretty = pretty . fromStrict

instance Pretty Int where
  pretty i =
    if i < 0
      then (parens . int) i
      else int i

instance Pretty Integer where
  pretty i =
    if i < 0
      then (parens . integer) i
      else integer i

instance Pretty Float where
  pretty i =
    if i < 0
      then (parens . float) i
      else float i

instance Pretty Double where
  pretty i =
    if i < 0
      then (parens . double) i
      else double i

instance Pretty Rational where
  pretty = rational

instance Pretty Bool where
  pretty = bool

instance Pretty ByteString where
  pretty = (pretty :: Text -> Doc) . cs

instance Pretty a =>
         Pretty [a] where
  pretty = brackets . align . fillCat . punctuate comma . fmap pretty

--   pretty (Just x) = nest 3 (text "Just" <$> pretty x)
instance Pretty a =>
         Pretty (Maybe a) where
  pretty Nothing  = text "Nothing"
  pretty (Just x) = text "Just" <+> pretty x

--   pretty (Left x)  = nest 3 ( text "Left" <$> pretty x)
--   pretty (Right y) = nest 3 ( text "Right" <$> pretty y)
instance (Pretty a, Pretty b) =>
         Pretty (Either a b) where
  pretty (Left x)  = text "Left" <$> pretty x
  pretty (Right y) = text "Right" <$> pretty y

instance (Pretty a, Pretty b) =>
         Pretty (a, b) where
  pretty (a, b) = tupled [pretty a, pretty b]

instance (Pretty a, Pretty b, Pretty c) =>
         Pretty (a, b, c) where
  pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) =>
         Pretty (a, b, c, d) where
  pretty (a, b, c, d) = tupled [pretty a, pretty b, pretty c, pretty d]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) =>
         Pretty (a, b, c, d, e) where
  pretty (a, b, c, d, e) =
    tupled [pretty a, pretty b, pretty c, pretty d, pretty e]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) =>
         Pretty (a, b, c, d, e, f) where
  pretty (a, b, c, d, e, f) =
    tupled [pretty a, pretty b, pretty c, pretty d, pretty e, pretty f]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) =>
         Pretty (a, b, c, d, e, f, g) where
  pretty (a, b, c, d, e, f, g) =
    tupled
      [pretty a, pretty b, pretty c, pretty d, pretty e, pretty f, pretty g]

instance (Pretty a, Pretty b) =>
         Pretty (Data.Map.Map a b) where
  pretty v = text "fromList" <+> (align . pretty) v

instance (Pretty a) =>
         Pretty (Data.IntMap.IntMap a) where
  pretty v = text "fromList" <+> (align . pretty) v

instance (Pretty a, Pretty b) =>
         Pretty (Data.HashMap.Strict.HashMap a b) where
  pretty v = text "fromList" <+> (align . pretty) v

instance Pretty UTCTime where
  pretty = text . cs . formatTime defaultTimeLocale rfc822DateFormat

instance (Show a, Indexable ixs a) =>
         Pretty (Data.IxSet.Typed.IxSet ixs a) where
  pretty = text . show

instance Pretty Word where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Word8 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Word16 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Word32 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Word64 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Int8 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Int16 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Int32 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

instance Pretty Int64 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral

displayPrettyLenL
  :: Pretty a
  => Int -> a -> Text
displayPrettyLenL l = PP.displayT . PP.renderPretty 1.0 l . pretty

displayPrettyLen
  :: Pretty a
  => Int -> a -> T.Text
displayPrettyLen l = toStrict . displayPrettyLenL l

displayPrettyL
  :: Pretty a
  => a -> Text
displayPrettyL = displayPrettyLenL 70

displayPretty
  :: Pretty a
  => a -> T.Text
displayPretty = toStrict . displayPrettyL

displayPrettyPrefixL
  :: (Pretty a, Pretty b)
  => a -> b -> Text
displayPrettyPrefixL prefix =
  PP.displayT . PP.renderPretty 1.0 70 . (PP.<>) (pretty prefix) . pretty

displayPrettyPrefix
  :: (Pretty a, Pretty b)
  => a -> b -> T.Text
displayPrettyPrefix prefix = toStrict . displayPrettyPrefixL prefix
