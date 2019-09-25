{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, TypeOperators #-}
module TreeSitter.Unmarshal.Examples () where

import qualified Data.Text as Text
import GHC.Generics ((:+:), Generic1)
import TreeSitter.Unmarshal

-- | An example of a sum-of-products datatype.
data Expr a
  = IfExpr (If a)
  | BlockExpr (Block a)
  | VarExpr (Var a)
  | BinExpr (Bin a)
  deriving (Generic1, Unmarshal)

-- | Product with multiple fields.
data If a = If { ann :: a, condition :: Expr a, consequence :: Expr a, alternative :: Maybe (Expr a) }
  deriving (Generic1, Unmarshal)

instance SymbolMatching If where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Single-field product.
data Block a = Block { ann :: a, body :: [Expr a] }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Block where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Leaf node.
data Var a = Var { ann :: a, text :: Text.Text }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Var where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Product with anonymous sum field.
data Bin a = Bin { ann :: a, lhs :: Expr a, op :: (AnonPlus :+: AnonTimes) a, rhs :: Expr a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching Bin where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Anonymous leaf node.
newtype AnonPlus a = AnonPlus { ann :: a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching AnonPlus where
  symbolMatch _ _ = False
  showFailure _ _ = ""

-- | Anonymous leaf node.
newtype AnonTimes a = AnonTimes { ann :: a }
  deriving (Generic1, Unmarshal)

instance SymbolMatching AnonTimes where
  symbolMatch _ _ = False
  showFailure _ _ = ""
