{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

import Protolude
import Data.String.Conversions
import Text.Groom
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint.GenericStructure
import Text.PrettyPrint.GenericPrettyStructure
import qualified Text.PrettyPrint.TestGenericPretty as Test
import Text.PrettyPrint.Leijen.Text hiding (Pretty)
import qualified Text.PrettyPrint.Leijen.Text as PP

data Tree a
  = Leaf a
  | Node (Tree a)
         (Tree a)
  deriving (Generic, Show, Pretty, Test.Pretty,Structure, PrettyStructure)

tree1 :: Tree Int
tree1 = (Leaf 333333)

tree2 :: Tree Int
tree2 = Node (Leaf 333333) (Leaf (-555555))

tree3 :: Tree Int
tree3 =
  Node
    (Node (Leaf 333333) (Leaf (-555555)))
    (Leaf 7777777)

tree4 :: Tree Int
tree4 =
  Node
    (Node (Leaf 3) (Leaf (-5)))
    (Node (Leaf 8) (Leaf 7))

tree5 :: Tree Int
tree5 =
  Node
    (Node (Leaf 333333) (Leaf (-555555)))
    (Node (Leaf 888888) (Leaf 57575757))

tree :: Tree Int
tree =
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
--   mapM_ ( putText . displayPretty) [ tree1, tree2 , tree3, tree4, tree5]
--   mapM_ (ppLen 30) [ tree1, tree2 , tree3, tree4, tree5]
--   mapM_ (ppLen 30) [ animal1 ]
--   (ppLen 30) [ 1 .. (100 :: Int)]
  putText "------ with TestGenericPretty -----"
  mapM_ (putText . Test.displayPrettyLen 30) [ tree1, tree2 , tree3, tree4, tree5]
  mapM_ (putText . Test.displayPrettyLen 30) [ animal1 ]
--   (putText . Test.displayPrettyLen 30) [ 1 .. (100 :: Int)]
--   ( putText . displayPretty) animal1
  putText "------ with Groom -------------"
  mapM_ ( putText . cs . groom) [ tree1, tree2 , tree3, tree4, tree5]
  mapM_ ( putText . cs . groom) [ animal1 ]
  putText "------ just Structure -------------"
--   mapM_ ( putText . displayStructure ) [ tree1, tree2 , tree3, tree4, tree5]
--   mapM_ ( putText . displayStructure) [ animal1 ]
  mapM_ ( putText . displayStructure) [ 100 :: Int ]
  mapM_ ( Protolude.putText . displayStructure) [ Leaf 100 :: Tree Int ]
  Protolude.putText "--- [ Node (Leaf 1) (Leaf 2) :: Tree Int ] ---"
  mapM_ ( Protolude.putText . displayStructure) [ Node (Leaf 1) (Leaf 2) :: Tree Int ]
  Protolude.putText "--- animal1 ---"
  (Protolude.putText . displayStructure) animal1
  mapM_ ( Protolude.putText . displayStructure) [ [ 1 .. (100 :: Int) ]]
  putText "------ Pretty Structure -------------"
  mapM_ ( putText . displayPrettyStructure) [ 100 :: Int ]
  mapM_ ( Protolude.putText . displayPrettyStructure) [ Leaf 100 :: Tree Int ]
  mapM_ ( Protolude.putText . displayPrettyStructure ) [ tree1, tree2 , tree3, tree4, tree5]
  mapM_ ( Protolude.putText . displayPrettyStructure) [ animal1 ]

  ( putText . displayStructure) tree
  (Protolude.putText . displayPrettyStructure ) tree
--   (ppLen 30) tree
  putText . Test.displayPrettyLen 30 $ tree
  putText . Test.displayPrettyLen 80 $ tree
  putText . Test.displayPrettyLen 80 $ animal1
  putText . displayPrettyLen 80 $ animal1
  ( putText . cs . groom) animal1

animal1 :: Animal1
animal1 = Animal1 10 "junk animal" "junk animal again" Dog tree

data Animal
  = Dog
  | Cat
  | Horse
  | Elephant
  deriving (Show, Enum, Generic, Pretty, Test.Pretty,Structure, PrettyStructure)

data Animal1 = Animal1
  { aType      :: Int
  , aName      :: Text
  , aNameAgain :: Text
  , animal     :: Animal
  , aTree      :: Tree Int
  } deriving (Show, Generic, Pretty, Test.Pretty,Structure, PrettyStructure)
