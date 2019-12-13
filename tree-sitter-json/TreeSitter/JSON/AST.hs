{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module TreeSitter.JSON.AST
( module TreeSitter.JSON.AST
, module TreeSitter.JSON.AST.Internal
, (GHC.Generics.:+:)(..)
) where

import qualified Data.Foldable
import qualified Data.Text.Internal.Text
import qualified Data.Traversable
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Generics
import qualified GHC.List
import qualified GHC.Real
import qualified GHC.Show
import Prelude hiding (String)
import TreeSitter.JSON.AST.Internal
import TreeSitter.Unmarshal


debugSymbolNames :: [String]
debugSymbolNames = ["end", "_{", "_,", "_}", "_:", "_[", "_]", "_\"", "_string_content_token1", "escape_sequence", "number", "true", "false", "null", "document", "_value", "object", "pair", "array", "string", "string_content", "_object_repeat1", "_array_repeat1", "_string_content_repeat1"]
newtype Value a_0
  = Value ((GHC.Generics.:+:) ((GHC.Generics.:+:) Array ((GHC.Generics.:+:) False Null)) ((GHC.Generics.:+:) ((GHC.Generics.:+:) Number Object) ((GHC.Generics.:+:) String True)) a_0)
    deriving newtype TreeSitter.Unmarshal.SymbolMatching
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
data Array a_1
    = Array {ann :: a_1, extraChildren :: (GHC.Types.[] (Value a_1))}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching Array
    where showFailure _ node_2 = "expected " <> ("\"array\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_2))))
          symbolMatch _ node_3 = elem (nodeSymbol node_3) [18]
data Document a_4
    = Document {ann :: a_4, extraChildren :: (Value a_4)}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching Document
    where showFailure _ node_5 = "expected " <> ("\"document\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_5))))
          symbolMatch _ node_6 = elem (nodeSymbol node_6) [14]
data Object a_7
    = Object {ann :: a_7, extraChildren :: (GHC.Types.[] (Pair a_7))}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching Object
    where showFailure _ node_8 = "expected " <> ("\"object\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_8))))
          symbolMatch _ node_9 = elem (nodeSymbol node_9) [16]
data Pair a_10
    = Pair {ann :: a_10, value :: (Value a_10), key :: ((GHC.Generics.:+:) Number String a_10)}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching Pair
    where showFailure _ node_11 = "expected " <> ("\"pair\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_11))))
          symbolMatch _ node_12 = elem (nodeSymbol node_12) [17]
data String a_13
    = String {ann :: a_13, extraChildren :: (GHC.Maybe.Maybe (StringContent a_13))}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching String
    where showFailure _ node_14 = "expected " <> ("\"string\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_14))))
          symbolMatch _ node_15 = elem (nodeSymbol node_15) [19]
type AnonymousDQuote = TreeSitter.Token.Token "\"" 7
type AnonymousComma = TreeSitter.Token.Token "," 2
type AnonymousColon = TreeSitter.Token.Token ":" 4
type AnonymousLBracket = TreeSitter.Token.Token "[" 5
type AnonymousRBracket = TreeSitter.Token.Token "]" 6
data EscapeSequence a_16
    = EscapeSequence {ann :: a_16, text :: Data.Text.Internal.Text}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching EscapeSequence
    where showFailure _ node_17 = "expected " <> ("\"escape_sequence\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_17))))
          symbolMatch _ node_18 = elem (nodeSymbol node_18) [9]
data False a_19
    = False {ann :: a_19, text :: Data.Text.Internal.Text}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching False
    where showFailure _ node_20 = "expected " <> ("\"false\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_20))))
          symbolMatch _ node_21 = elem (nodeSymbol node_21) [12]
data Null a_22
    = Null {ann :: a_22, text :: Data.Text.Internal.Text}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching Null
    where showFailure _ node_23 = "expected " <> ("\"null\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_23))))
          symbolMatch _ node_24 = elem (nodeSymbol node_24) [13]
data Number a_25
    = Number {ann :: a_25, text :: Data.Text.Internal.Text}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching Number
    where showFailure _ node_26 = "expected " <> ("\"number\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_26))))
          symbolMatch _ node_27 = elem (nodeSymbol node_27) [10]
data True a_28
    = True {ann :: a_28, text :: Data.Text.Internal.Text}
    deriving stock (GHC.Classes.Eq, GHC.Classes.Ord, GHC.Show.Show, GHC.Generics.Generic, Data.Foldable.Foldable, GHC.Base.Functor, Data.Traversable.Traversable, GHC.Generics.Generic1)
    deriving anyclass TreeSitter.Unmarshal.Unmarshal
instance SymbolMatching True
    where showFailure _ node_29 = "expected " <> ("\"true\"" <> (" but got " <> show (debugSymbolNames !! fromIntegral (nodeSymbol node_29))))
          symbolMatch _ node_30 = elem (nodeSymbol node_30) [11]
type AnonymousLBrace = TreeSitter.Token.Token "{" 1
type AnonymousRBrace = TreeSitter.Token.Token "}" 3
