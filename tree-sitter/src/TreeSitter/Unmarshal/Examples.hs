{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TypeOperators #-}
module TreeSitter.Unmarshal.Examples () where

import qualified Data.Text as Text
import GHC.Generics ((:+:), Generic)
import TreeSitter.Unmarshal

-- | An example of a sum-of-products datatype.
data Expr a
  = IfExpr (If a)
  | BlockExpr (Block a)
  | LitExpr (Lit a)
  | BinExpr (Bin a)
  deriving (Generic, Unmarshal)

-- | Product with multiple fields.
data If a = If { ann :: a, condition :: Expr a, consequence :: Expr a, alternative :: Expr a }
  deriving (Generic, Unmarshal)

instance SymbolMatching (If a) where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Single-field product.
data Block a = Block { ann :: a, body :: [Expr a] }
  deriving (Generic, Unmarshal)

instance SymbolMatching (Block a) where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Leaf node.
data Lit a = Lit { ann :: a, text :: Text.Text }
  deriving (Generic, Unmarshal)

instance SymbolMatching (Lit a) where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Product with anonymous sum field.
data Bin a = Bin { ann :: a, lhs :: Expr a, op :: (AnonPlus :+: AnonTimes) a, rhs :: Expr a }
  deriving (Generic, Unmarshal)

instance SymbolMatching (Bin a) where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Anonymous leaf node.
newtype AnonPlus a = AnonPlus { ann :: a }
  deriving (Generic, Unmarshal)

instance SymbolMatching (AnonPlus a) where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Anonymous leaf node.
newtype AnonTimes a = AnonTimes { ann :: a }
  deriving (Generic, Unmarshal)

instance SymbolMatching (AnonTimes a) where
  symbolMatch _ _ = False
  showFailure _ _ = ""
