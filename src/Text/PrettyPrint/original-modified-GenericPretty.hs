{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

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
  , pp
  , ppLen
  , ppStyle
  , displayPretty
  , displayPrettyL
  ) where

import           Data.Char
import           Data.List                    (last)
import qualified Data.Monoid                  as Monoid
import           Data.String.Conversions      (cs)
import qualified Data.Text                    as T
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as LT
import qualified Data.Text.Lazy.IO            as LT
import           GHC.Generics
import           Protolude                    hiding (Text, Type,
                                               empty, (<>))
import           Text.PrettyPrint.Leijen.Text hiding (Pretty, pretty, prettyList)
import qualified Text.PrettyPrint.Leijen.Text as PP
import           Data.IxSet.Typed             (Indexable)
import qualified Data.Map
import           Data.Time
import qualified Data.HashMap.Strict
import qualified Data.IntMap
import qualified Data.IxSet.Typed

-- | The class 'Pretty' is the equivalent of 'Prelude.Show'
--
-- It provides conversion of values to pretty printable Pretty.Doc's.
--
-- Minimal complete definition: 'prettyPrec' or 'doc'.
--
-- Derived instances of 'Pretty' have the following properties
--
-- * The result of 'prettyPrec' is a syntactically correct Haskell
--   expression containing only constants, given the fixity
--   declarations in force at the point where the type is declared.
--   It contains only the constructor names defined in the data type,
--   parentheses, and spaces.  When labelled constructor fields are
--   used, braces, commas, field names, and equal signs are also used.
--
-- * If the constructor is defined to be an infix operator, then
--   'prettyPrec' will produce infix applications of the constructor.
--
-- * the representation will be enclosed in parentheses if the
--   precedence of the top-level constructor in @x@ is less than @d@
--   (associativity is ignored).  Thus, if @d@ is @0@ then the result
--   is never surrounded in parentheses; if @d@ is @11@ it is always
--   surrounded in parentheses, unless it is an atomic expression.
--
-- * If the constructor is defined using record syntax, then 'prettyPrec'
--   will produce the record-syntax form, with the fields given in the
--   same order as the original declaration.
--
-- For example, given the declarations
--
--
-- > data Tree a =  Leaf a  |  Node (Tree a) (Tree a) deriving (Generic)
--
-- The derived instance of 'Pretty' is equivalent to:
--
-- > instance (Pretty a) => Pretty (Tree a) where
-- >
-- >         prettyPrec d (Leaf m) = Pretty.sep $ wrapParens (d > appPrec) $
-- >              text "Leaf" : [nest (constrLen + parenLen) (prettyPrec (appPrec+1) m)]
-- >           where appPrec = 10
-- >                 constrLen = 5;
-- >                 parenLen = if(d > appPrec) then 1 else 0
-- >
-- >         prettyPrec d (Node u v) = Pretty.sep $ wrapParens (d > appPrec) $
-- >              text "Node" :
-- >              nest (constrLen + parenLen) (prettyPrec (appPrec+1) u) :
-- >              [nest (constrLen + parenLen) (prettyPrec (appPrec+1) v)]
-- >           where appPrec = 10
-- >                 constrLen = 5
-- >                 parenLen = if(d > appPrec) then 1 else 0
class Pretty a
      -- | 'prettyPrec' is the equivalent of 'Prelude.showsPrec'.
      --
      -- Convert a value to a pretty printable 'Pretty.Doc'.
                                                             where
  prettyPrec
    :: Int -- ^ the operator precedence of the enclosing
       -- context (a number from @0@ to @11@).
       -- Function application has precedence @10@.
    -> a -- ^ the value to be converted to a 'String'
    -> Doc -- ^ the resulting Doc
  default prettyPrec :: (Generic a, GPretty (Rep a)) =>
    Int -> a -> Doc
--   prettyPrec n x = sep $ gpretty (from x) Pref n False
  prettyPrec n x = case (gpretty . from) x Pref n False of
                    (o:[]) -> o
                    os -> PP.list os
  -- | 'doc' is the equivalent of 'Prelude.show'
  --
  -- This is a specialised variant of 'prettyPrec', using precedence context zero.
  pretty :: a -> Doc
  default pretty :: (Generic a, GPretty (Rep a)) =>
    a -> Doc
  pretty x = sep $ gpretty (from x) Pref 0 False
  -- | 'prettyList' is the equivalent of 'Prelude.showList'.
  --
  -- The method 'prettyList' is provided to allow the programmer to
  -- give a specialised way of showing lists of values.
  -- For example, this is used by the predefined 'Pretty' instance of
  -- the 'Char' type, where values of type 'String' should be shown
  -- in double quotes, rather than between square brackets.
  prettyList :: [a] -> Doc
  prettyList = prettyListWith pretty

-- used to define prettyList, creates output identical to that of show for general list types
prettyListWith :: (a -> Doc) -> [a] -> Doc
prettyListWith f = brackets . fillCat . punctuate comma . map f

-- returns a list without it's first and last elements
-- except if the list has a single element, in which case it returns the list unchanged
middle :: [a] -> [a]
middle []     = []
middle [x]    = [x]
middle (_:xs) = initDef [] xs

-- |Utility function used to wrap the passed value in parens if the bool is true.
wrapParens :: Bool -> [Doc] -> [Doc]
wrapParens _ [] = []
wrapParens False s = s
wrapParens True s
  | length s == 1 = [lparen <> (fromMaybe empty . head) s <> rparen]
  | otherwise =
    [lparen <> (fromMaybe empty . head) s] ++ middle s ++ [last s <> rparen]

-- show the whole document in one line
showDocOneLine :: Doc -> Text
showDocOneLine = displayT . renderOneLine

-- The types of data we need to consider for product operator. Record, Prefix and Infix.
-- Tuples aren't considered since they're already instances of 'Pretty' and thus won't pass through that code.
data Type
  = Rec
  | Pref
  | Inf Text

--'GPretty' is a helper class used to output the Sum-of-Products type, since it has kind *->*,
-- so can't be an instance of 'Pretty'
class GPretty f
      -- |'gpretty' is the (*->*) kind equivalent of 'prettyPrec'
                                                            where
  gpretty
    :: f x -- The sum of products representation of the user's custom type
    -> Type -- The type of multiplication. Record, Prefix or Infix.
    -> Int -- The operator precedence, determines wether to wrap stuff in parens.
    -> Bool -- A flag, marks wether the constructor directly above was wrapped in parens.
       -- Used to determine correct indentation
    -> [Doc] -- The result. Each Doc could be on a newline, depending on available space.
  -- |'isNullary' marks nullary constructors, so that we don't put parens around them
  isNullary :: f x -> Bool

-- if empty, output nothing, this is a null constructor
instance GPretty U1 where
  gpretty _ _ _ _ = []
  isNullary _ = True

-- ignore datatype meta-information
instance (GPretty f) =>
         GPretty (M1 D c f) where
  gpretty (M1 a) t i b = (text "M1 D") : gpretty a t i b
  isNullary (M1 a) = isNullary a

-- if there is a selector, display it and it's value + appropriate white space
instance (GPretty f, Selector c) =>
         GPretty (M1 S c f) where
  gpretty s@(M1 a) t d p
    | LT.null selector = gpretty a t d p
    | otherwise =
--       (text "M1 S" <+> string selector <+> char '=') :
--       map (nest ((fromIntegral . LT.length) selector + 3)) (gpretty a t 0 p)
      [ text "M1 S" <+> string selector <+> char '=' <+>
        (nest ((fromIntegral . LT.length) selector + 3) . cat) (gpretty a t 0 p)]
    where
      selector = (identity . cs . selName) s
  isNullary (M1 a) = isNullary a

-- constructor
-- here the real type and parens flag is set and propagated forward via t and n, the precedence factor is updated
instance (GPretty f, Constructor c) =>
         GPretty (M1 C c f) where
  gpretty c@(M1 a) _ d _ =
    case fixity
         -- if prefix add the constructor name, nest the result and possibly put it in parens
          of
      Prefix ->
--         (string "M1 C Prefix ") : (wrapParens boolParens $ text name : makeMargins t boolParens (gpretty a t 11 boolParens))
           wrapParens boolParens [string "M1 C Prefix " <+> (string . cs . conName) c <+> cat (makeMargins t boolParens (gpretty a t 11 boolParens))]
      -- if infix possibly put in parens
      Infix _ m -> (string "M1 C Infix ") : (wrapParens (d > m) $ gpretty a t (m + 1) (d > m))
    where
      boolParens = d > 10 && (not $ isNullary a)
      name = checkInfix . cs $ conName c
      fixity = conFixity c
      -- get the type of the data, Record, Infix or Prefix.
      t =
        if conIsRecord c
          then Rec
          else case fixity of
                 Prefix    -> Pref
                 Infix _ _ -> (Inf . cs . conName) c
      --add whitespace and possible braces for records
      makeMargins :: Type -> Bool -> [Doc] -> [Doc]
      makeMargins _ _ [] = []
      makeMargins Rec _ s
        | length s == 1 =
          [ nest
              ((identity . fromIntegral . LT.length) name + 1)
              (lbrace <> (fromMaybe empty . head) s <> rbrace)
          ]
        | otherwise =
          nest
            ((identity . fromIntegral . LT.length) name + 1)
            (lbrace <> (fromMaybe empty (head s))) :
          map
            (nest $ (identity . fromIntegral . LT.length) name + 2)
            (middle s ++ [last s <> rbrace])
      makeMargins _ b s =
        map
          (nest $
           ((identity . fromIntegral . LT.length) name) +
           if b
             then 2
             else 1)
          s
      -- check for infix operators that are acting like prefix ones due to records, put them in parens
      checkInfix :: Text -> Text
      checkInfix xs
        | xs == LT.empty = LT.empty
        | otherwise =
          let x = LT.head xs
          in if fixity == Prefix && (isAlphaNum x || x == '_')
               then xs
               else "(" Monoid.<> xs Monoid.<> ")"
  isNullary (M1 a) = isNullary a

-- ignore tagging, call prettyPrec since these are concrete types
instance (Pretty f) =>
         GPretty (K1 t f) where
  gpretty (K1 a) _ d _ = [prettyPrec d a]
  isNullary _ = False

-- just continue to the corresponding side of the OR
instance (GPretty f, GPretty g) =>
         GPretty (f :+: g) where
  gpretty (L1 a) t d p =  gpretty a t d p
  gpretty (R1 a) t d p = gpretty a t d p
  isNullary (L1 a) = isNullary a
  isNullary (R1 a) = isNullary a

-- output both sides of the product, possible separated by a comma or an infix operator
instance (GPretty f, GPretty g) =>
         GPretty (f :*: g) where
  gpretty (f :*: g) t@Rec d p = initDef [] pfn ++ [last pfn <> comma] ++ pgn
    where
      pfn = gpretty f t d p
      pgn = gpretty g t d p
  -- if infix, nest the second value since it isn't nested in the constructor
  gpretty (f :*: g) t@(Inf s) d p =
    initDef [] pfn ++ [last pfn <+> text s] ++ checkIndent pgn
    where
      pfn = gpretty f t d p
      pgn = gpretty g t d p
      -- if the second value of the :*: is in parens, nest it, otherwise just check for an extra paren space
      -- needs to get the string representation of the first elements in the left and right Doc lists
      -- to be able to determine the correct indentation
      checkIndent :: [Doc] -> [Doc]
      checkIndent [] = []
      checkIndent m@(x:_)
        | parensLength == 0 =
          if p
            then map (nest 1) m
            else m
        | otherwise = map (nest $ fromIntegral cons + 1 + parenSpace) m
        where
          parenSpace =
            if p
              then 1
              else 0
          strG = showDocOneLine x
          cons =
            maybe
              0
              (LT.length .
               LT.takeWhile (/= ' ') . LT.dropWhile (== '(') . showDocOneLine)
              (head pfn)
          parensLength = LT.length $ LT.takeWhile (== '(') strG
  gpretty (f :*: g) t@Pref n p = gpretty f t n p ++ gpretty g t n p
  isNullary _ = False

-- | 'fullPP' is a fully customizable Pretty Printer
--
-- Every other pretty printer just gives some default values to 'fullPP'
-- fullPP
--   :: (Pretty a)
--   => (TextDetails -> b -> b) -- ^Function that handles the text conversion /(eg: 'outputIO')/
--   -> b -- ^The end element of the result /( eg: "" or putChar('\n') )/
--   -> Style -- ^The pretty printing 'Text.PrettyPrint.MyPretty.Style' to use
--   -> a -- ^The value to pretty print
--   -> b -- ^The pretty printed result
-- fullPP td end s a =
--   fullRender (mode s) (lineLength s) (ribbonsPerLine s) td end doc
--   where
--     pretty = prettyPrec 0 a
-- | Utility function that handles the text conversion for 'fullPP'.
--
-- 'outputIO' transforms the text into 'String's and outputs it directly.
-- outputIO :: TextDetails -> IO () -> IO ()
-- outputIO td act = do
--   putStr $ decode td
--   act
--   where
--     decode :: TextDetails -> String
--     decode (Str s)   = s
--     decode (PStr s1) = s1
--     decode (Chr c)   = [c]
-- | Utility function that handles the text conversion for 'fullPP'.
--
--'outputStr' just leaves the text as a 'String' which is usefull if you want
-- to further process the pretty printed result.
-- outputStr :: TextDetails -> String -> String
-- outputStr td str = decode td ++ str
--   where
--     decode :: TextDetails -> String
--     decode (Str s)   = s
--     decode (PStr s1) = s1
--     decode (Chr c)   = [c]
-- | Customizable pretty printer
--
-- Takes a user defined 'Text.PrettyPrint.MyPretty.Style' as a parameter and uses 'outputStr' to obtain the result
-- Equivalent to:
--
-- > fullPP outputStr ""
prettyStyle
  :: (Pretty a)
  => Float -> Int -> a -> Text
prettyStyle r l = displayT . renderPretty r l . pretty

-- | Semi-customizable pretty printer.
--
-- Equivalent to:
--
-- > prettyStyle customStyle
--
-- Where customStyle uses the specified line length, mode = PageMode and ribbonsPerLine = 1.
prettyLen
  :: (Pretty a)
  => Int -> a -> Text
prettyLen l = displayT . renderPretty 1.0 l . pretty

-- | The default pretty printer returning 'String's
--
--  Equivalent to
--
-- > prettyStyle defaultStyle
--
-- Where defaultStyle = (mode=PageMode, lineLength=80, ribbonsPerLine=1.5)
-- pretty
--   :: (Pretty a)
--   => a -> Text
-- pretty = displayT . renderPretty 1.0 80 . pretty
displayPrettyL
  :: Pretty a
  => a -> Text
displayPrettyL = displayT . renderPretty 1.0 70 . pretty -- pretty

displayPretty
  :: Pretty a
  => a -> T.Text
displayPretty = toStrict . displayPrettyL

-- | Customizable pretty printer.
--
-- Takes a user defined 'Text.PrettyPrint.MyPretty.Style' as a parameter and uses 'outputIO' to obtain the result
-- Equivalent to:
--
-- > fullPP outputIO (putChar '\n')
ppStyle
  :: (Pretty a)
  => Float -> Int -> a -> IO ()
ppStyle r l = LT.putStrLn . prettyStyle r l

-- | Semi-customizable pretty printer.
--
-- Equivalent to:
--
-- > ppStyle customStyle
--
-- Where customStyle uses the specified line length, mode = PageMode and ribbonsPerLine = 1.
ppLen
  :: (Pretty a)
  => Int -> a -> IO ()
ppLen l = LT.putStrLn . prettyLen l

-- | The default Pretty Printer,
--
--  Equivalent to:
--
-- > ppStyle defaultStyle
--
-- Where defaultStyle = (mode=PageMode, lineLength=80, ribbonsPerLine=1.5)
pp
  :: (Pretty a)
  => a -> IO ()
pp = putDoc . pretty

-- define some instances of Pretty making sure to generate output identical to 'show' modulo the extra whitespace
instance Pretty () where
  pretty _ = text "()"
  prettyPrec _ = pretty

instance Pretty Char where
  pretty a = char '\'' <> (text . LT.singleton $ a) <> char '\''
  prettyPrec _ = pretty
  prettyList = text . cs

instance Pretty Int where
  prettyPrec n x
    | n /= 0 && x < 0 = parens $ int x
    | otherwise = int x
  pretty = prettyPrec 0

instance Pretty Integer where
  prettyPrec n x
    | n /= 0 && x < 0 = parens $ integer x
    | otherwise = integer x
  pretty = prettyPrec 0

instance Pretty Float where
  prettyPrec n x
    | n /= 0 && x < 0 = parens $ float x
    | otherwise = float x
  pretty = prettyPrec 0

instance Pretty Double where
  prettyPrec n x
    | n /= 0 && x < 0 = parens $ double x
    | otherwise = double x
  pretty = prettyPrec 0

instance Pretty Rational where
  prettyPrec n x
    | n /= 0 && x < 0 = parens $ rational x
    | otherwise = rational x
  pretty = prettyPrec 0

instance Pretty a =>
         Pretty [a] where
  pretty = prettyList
  prettyPrec _ = pretty

instance Pretty Bool where
  pretty True  = text "True"
  pretty False = text "False"
  prettyPrec _ = pretty

instance Pretty a =>
         Pretty (Maybe a) where
  prettyPrec _ Nothing = text "Nothing"
  prettyPrec n (Just x)
    | n /= 0 = parens result
    | otherwise = result
    where
      result = text "Just" <+> prettyPrec 10 x
  pretty = prettyPrec 0

instance (Pretty a, Pretty b) =>
         Pretty (Either a b) where
  prettyPrec n (Left x)
    | n /= 0 = parens result
    | otherwise = result
    where
      result = string "Left" <+> prettyPrec 10 x
  prettyPrec n (Right y)
    | n /= 0 = parens result
    | otherwise = result
    where
      result = string "Right" <+> prettyPrec 10 y
  pretty = prettyPrec 0

instance (Pretty a, Pretty b) =>
         Pretty (a, b) where
  pretty (a, b) = parens (sep [pretty a <> comma, pretty b])
  prettyPrec _ = pretty

instance (Pretty a, Pretty b, Pretty c) =>
         Pretty (a, b, c) where
  pretty (a, b, c) = parens (sep [pretty a <> comma, pretty b <> comma, pretty c])
  prettyPrec _ = pretty

instance (Pretty a, Pretty b, Pretty c, Pretty d) =>
         Pretty (a, b, c, d) where
  pretty (a, b, c, d) =
    parens (sep [pretty a <> comma, pretty b <> comma, pretty c <> comma, pretty d])
  prettyPrec _ = pretty

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) =>
         Pretty (a, b, c, d, e) where
  pretty (a, b, c, d, e) =
    parens
      (sep
         [pretty a <> comma, pretty b <> comma, pretty c <> comma, pretty d <> comma, pretty e])
  prettyPrec _ = pretty

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) =>
         Pretty (a, b, c, d, e, f) where
  pretty (a, b, c, d, e, f) =
    parens
      (sep
         [ pretty a <> comma
         , pretty b <> comma
         , pretty c <> comma
         , pretty d <> comma
         , pretty e <> comma
         , pretty f
         ])
  prettyPrec _ = pretty

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) =>
         Pretty (a, b, c, d, e, f, g) where
  pretty (a, b, c, d, e, f, g) =
    parens
      (sep
         [ pretty a <> comma
         , pretty b <> comma
         , pretty c <> comma
         , pretty d <> comma
         , pretty e <> comma
         , pretty f <> comma
         , pretty g
         ])
  prettyPrec _ = pretty

instance Pretty LT.Text where
  pretty = string
  prettyPrec _ = pretty
  prettyList = pretty

instance Pretty T.Text where
  pretty = string . fromStrict
  prettyPrec _ = pretty
  prettyList = pretty

instance (Pretty a, Pretty b) =>
         Pretty (Data.Map.Map a b) where
  pretty v = text "fromList " <+> pretty v
  prettyPrec _ = pretty

instance (Pretty a) =>
         Pretty (Data.IntMap.IntMap a) where
  pretty v = text "fromList " <+> pretty v
  prettyPrec _ = pretty

instance (Pretty a, Pretty b) =>
         Pretty (Data.HashMap.Strict.HashMap a b) where
  pretty v = text "fromList " <+> pretty v
  prettyPrec _ = pretty

instance Pretty UTCTime where
  pretty = text . cs . formatTime defaultTimeLocale rfc822DateFormat
  prettyPrec _ = pretty

instance (Show a, Indexable ixs a) =>
         Pretty (Data.IxSet.Typed.IxSet ixs a) where
  pretty = text . show
  prettyPrec _ = pretty

instance Pretty Word where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Word8 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Word16 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Word32 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Word64 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Int8 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Int16 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Int32 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty

instance Pretty Int64 where
  pretty = (pretty :: Integer -> Doc) . fromIntegral
  prettyPrec _ = pretty
