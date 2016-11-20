{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude hiding (Text)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as TL
import qualified Text.PrettyPrint.Leijen.Text as PP

import           Text.PrettyPrint.GenericPretty

data Expr
  = Var Text
  | Lit Lit
  | App Expr Expr
  | Lam Text Expr deriving (Generic,Pretty)

data Lit
  = LInt Int
  | LBool Bool deriving (Generic,Pretty)

ppexpr :: Expr -> Text
ppexpr x = PP.displayT (PP.renderPretty 1.0 70 (pretty x))

s, k, example :: Expr
s = Lam "f" (Lam "g" (Lam "x" (App (Var "f") (App (Var "g") (Var "x")))))
k = Lam "x" (Lam "y" (Var "x"))
example = App s k

main :: IO ()
main = do
  TL.putStrLn (ppexpr s)
  TL.putStrLn (ppexpr k)
