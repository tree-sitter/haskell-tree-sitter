{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TypeOperators #-}
module TreeSitter.Unmarshal.Examples () where

import qualified Data.Text as Text
import GHC.Generics ((:+:), Generic1)
import TreeSitter.Unmarshal

data Expr a
  = IfExpr (If a)
  | BlockExpr (Block a)
  | LitExpr (Lit a)
  | BinExpr (Bin a)
  deriving (Generic1, Unmarshal)

data If a = If { ann :: a, condition :: Expr a, consequence :: Expr a, alternative :: Expr a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching If where
  symbolMatch _ _ = False
  showFailure _ _ = ""

data Block a = Block { ann :: a, body :: [Expr a] }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Block where
  symbolMatch _ _ = False
  showFailure _ _ = ""

data Lit a = Lit { ann :: a, text :: Text.Text }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Lit where
  symbolMatch _ _ = False
  showFailure _ _ = ""

data Bin a = Bin { ann :: a, lhs :: Expr a, op :: (AnonPlus :+: AnonTimes) a, rhs :: Expr a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Bin where
  symbolMatch _ _ = False
  showFailure _ _ = ""

newtype AnonPlus a = AnonPlus { ann :: a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching AnonPlus where
  symbolMatch _ _ = False
  showFailure _ _ = ""

newtype AnonTimes a = AnonTimes { ann :: a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching AnonTimes where
  symbolMatch _ _ = False
  showFailure _ _ = ""
