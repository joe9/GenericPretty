{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

import Protolude
import Data.String.Conversions
import Text.Groom
import Text.PrettyPrint.GenericPretty

data Tree a
  = Leaf a
  | Node (Tree a)
         (Tree a)
  deriving (Generic, Show, Pretty)

tree1 :: Tree Int
tree1 =
  Node
    (Node (Leaf 333333) (Leaf (-555555)))
    (Node
       (Node (Node (Leaf 888888) (Leaf 57575757)) (Leaf (-14141414)))
       (Leaf 7777777))

-- zigStyle :: Style
-- zigStyle = Style {mode = ZigZagMode, lineLength = 30, ribbonsPerLine = 1.5}

main :: IO ()
main = do
--   ppStyle zigStyle tree1
  putText "------ with GenericPretty -----"
  ( putText . displayPretty) tree1
  ( putText . displayPretty) animal1
  putText "------ with Groom -------------"
  (putText . cs . groom) animal1

animal1 :: Animal1
animal1 = Animal1 10 "junk animal" "junk animal again" Dog tree1

data Animal
  = Dog
  | Cat
  | Horse
  | Elephant
  deriving (Show, Enum, Generic, Pretty)

data Animal1 = Animal1
  { aType      :: Int
  , aName      :: Text
  , aNameAgain :: Text
  , animal     :: Animal
  , aTree      :: Tree Int
  } deriving (Show, Generic, Pretty)

