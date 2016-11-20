{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Text.Lazy.IO              as TL
import           Protolude                      hiding (Text, (<>))
import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint.Leijen.Text   hiding (Pretty (..),
                                                 pretty)
import qualified Text.PrettyPrint.Leijen.Text   as PP

data Tree a
  = Leaf a
  | Node (Tree a)
         (Tree a)
  deriving (Generic)

instance (Pretty a) =>
         Pretty (Tree a) where
  pretty (Leaf a) = parens $ text "customLeaf" <+> pretty a
  pretty (Node a b) =
    parens $ text "customNode" <$$> nest 1 (pretty a) <$$> nest 1 (pretty b)

tree1 :: Tree Int
tree1 =
  Node
    (Node (Leaf 333333) (Leaf (-555555)))
    (Node
       (Node (Node (Leaf 888888) (Leaf 57575757)) (Leaf (-14141414)))
       (Leaf 7777777))

main :: IO ()
main = TL.putStrLn (PP.displayT (PP.renderPretty 1.0 70 (pretty tree1)))
