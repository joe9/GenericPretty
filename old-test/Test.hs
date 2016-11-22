{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.Lazy               (Text)
import           qualified Data.Text.IO               as T
import           Data.String.Conversions (cs)
import           Text.Groom
import           Data.Text.Lazy.IO            as TL
import           Protolude                    hiding (Text, (<>))
import           GHC.Generics
import qualified Text.PrettyPrint.Leijen.Text as PP

import Text.PrettyPrint.GenericPretty

data Tree a
  = Leaf a
  | Node (Tree a)
         (Tree a)
  deriving (Eq, Read, Show, Generic, Pretty)

data Tree1 a b
  = Leaf1 a
          b
  | Node1 (Tree1 a b)
          (Tree1 a b)
  deriving (Generic, Pretty)

tree1 :: Tree Int
tree1 =
  Node
    (Node (Leaf 333333) (Leaf (-555555)))
    (Node
       (Node (Node (Leaf 888888) (Leaf 57575757)) (Leaf (-14141414)))
       (Leaf 7777777))

tree2 :: Tree Int
tree2 = Leaf 7777777

tree3 :: Tree Int
tree3 = Node (Leaf 7777777) (Leaf 5555555)

tree4 :: Tree1 Int Text
tree4 =
  Node1
    (Node1 (Leaf1 333333 "testing agan") (Leaf1 (-555555) "ntest"))
    (Node1
       (Node1
          (Node1 (Leaf1 888888 "again") (Leaf1 57575757 "tisti"))
          (Leaf1 (-14141414) "test"))
       (Leaf1 7777777 "aointhk"))

ppexpr :: Tree Int -> Text
ppexpr x = PP.displayT (PP.renderPretty 1.0 70 (pretty x))

ppexpr2 :: Tree1 Int Text -> Text
ppexpr2 x = PP.displayT (PP.renderPretty 1.0 70 (pretty x))

main :: IO ()
main = do
  TL.putStrLn (ppexpr tree1)
  (T.putStrLn . cs . groom) tree1
  TL.putStrLn (ppexpr1 Dog)
  TL.putStrLn (ppexpr tree2)
  TL.putStrLn (ppexpr tree3)
  TL.putStrLn (ppexpr2 tree4)
  TL.putStrLn
    (PP.displayT (PP.renderPretty 1.0 70 (pretty (Just 10 :: Maybe Int))))
  TL.putStrLn
    (PP.displayT (PP.renderPretty 1.0 70 (pretty ([10, 20 .. 1000] :: [Int]))))
  TL.putStrLn
    (PP.displayT (PP.renderPretty 1.0 70 (pretty (Just ([10, 20 .. 1000] :: [Int])))))
  TL.putStrLn (ppexpr3 animal1)
  (T.putStrLn . cs . groom) animal1

animal1 :: Animal1
animal1 = Animal1 10 "junk animal" "junk animal again" Dog tree1 'C'

data Animal
  = Dog
  | Cat
  | Horse
  | Elephant
  deriving (Eq, Read, Show, Generic, Pretty, Enum)

ppexpr1 :: Animal -> Text
ppexpr1 x = PP.displayT (PP.renderPretty 1.0 70 (pretty x))

data Animal1 = Animal1
  { aType      :: Int
  , aName      :: Text
  , aNameAgain :: Text
  , animal     :: Animal
  , aTree      :: Tree Int
  , aChar      :: Char
  } deriving (Eq, Read, Show, Generic, Pretty)

ppexpr3 :: Animal1 -> Text
ppexpr3 x = PP.displayT (PP.renderPretty 1.0 70 (pretty x))
