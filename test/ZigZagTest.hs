{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

import Protolude
import Data.Text.IO hiding (putStrLn)
import Text.PrettyPrint.Leijen.Text
import Data.String.Conversions
import Text.Groom
import Text.PrettyPrint.GenericPretty

data Tree a
  = Leaf a
  | Node (Tree a)
         (Tree a)
  deriving (Generic, Show, Out)

tree1 :: Tree Int
tree1 =
  Node
    (Node (Leaf 333333) (Leaf (-555555)))
    (Node
       (Node (Node (Leaf 888888) (Leaf 57575757)) (Leaf (-14141414)))
       (Leaf 7777777))

zigStyle :: Style
zigStyle = Style {mode = ZigZagMode, lineLength = 30, ribbonsPerLine = 1.5}

main :: IO ()
main = do
--   ppStyle zigStyle tree1
  putText "------ with GenericPretty -----"
  ppStyle (Style {mode = PageMode, lineLength = 100, ribbonsPerLine = 1.5}) tree1
  ppStyle (Style {mode = PageMode, lineLength = 100, ribbonsPerLine = 1.5}) animal1
  putText "------ with Groom -------------"
  (putText . cs . groom) animal1

animal1 :: Animal1
animal1 = Animal1 10 "junk animal" "junk animal again" Dog tree1

data Animal
  = Dog
  | Cat
  | Horse
  | Elephant
  deriving (Show, Enum, Generic, Out)

data Animal1 = Animal1
  { aType      :: Int
  , aName      :: Text
  , aNameAgain :: Text
  , animal     :: Animal
  , aTree      :: Tree Int
  } deriving (Show, Generic, Out)

