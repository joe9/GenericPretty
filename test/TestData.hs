{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestData where

import Data.String.Conversions (cs)
import GHC.Base
import GHC.Generics
import Protolude               hiding (K1)
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.IntMap.Strict      as IntMap
import qualified Data.Map.Strict      as Map

import Text.PrettyPrint.GenericPretty

showAndReadTree :: Tree Int -> Either GHC.Base.String (Tree Int)
showAndReadTree = readEither . cs . displayPretty

showAndReadTree1 :: Tree1 Int Text -> Either GHC.Base.String (Tree1 Int Text)
showAndReadTree1 = readEither . cs . displayPretty

showAndReadAnimal1 :: Animal1 -> Either GHC.Base.String Animal1
showAndReadAnimal1 = readEither . cs . displayPretty

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
  deriving (Eq, Read, Show, Generic, Pretty)

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

animal1 :: Animal1
animal1 =
  Animal1 10 "junk animal" "junk animal again" Dog tree1 'C' "test bytestring"

data Animal
  = Dog
  | Cat
  | Horse
  | Elephant
  deriving (Eq, Read, Show, Generic, Pretty, Enum)

data Animal1 = Animal1
  { aType      :: Int
  , aName      :: Text
  , aNameAgain :: Text
  , animal     :: Animal
  , aTree      :: Tree Int
  , aChar      :: Char
  , aBS        :: ByteString
  } deriving (Eq, Read, Show, Generic, Pretty)

hashMapTest :: HashMap.HashMap Int Text
hashMapTest = HashMap.fromList [(1, "10")]

showAndReadHashMap :: HashMap.HashMap Int Text -> Either GHC.Base.String ( HashMap.HashMap Int Text)
showAndReadHashMap = readEither . cs . displayPretty

intMapTest :: IntMap.IntMap Text
intMapTest = IntMap.fromList [(1, "10")]

showAndReadIntMap :: IntMap.IntMap Text -> Either GHC.Base.String ( IntMap.IntMap Text)
showAndReadIntMap = readEither . cs . displayPretty

mapTest :: Map.Map Int Text
mapTest = Map.fromList [(1, "10")]

showAndReadMap :: Map.Map Int Text -> Either GHC.Base.String ( Map.Map Int Text)
showAndReadMap = readEither . cs . displayPretty
