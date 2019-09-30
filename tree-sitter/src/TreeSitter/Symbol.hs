{-# LANGUAGE DeriveLift, ScopedTypeVariables, LambdaCase #-}
module TreeSitter.Symbol where

import Data.Char (isAlpha, toUpper, isControl)
import Data.Function ((&))
import Data.Ix (Ix)
import Data.List.Split (condense, split, whenElt)
import Data.Word (Word16)
import Language.Haskell.TH.Syntax

type TSSymbol = Word16

-- | Map a 'TSSymbol' to the corresponding value of a 'Symbol' datatype.
--
--   This should be used instead of 'toEnum' to perform this conversion, because tree-sitter represents parse errors with the unsigned short @65535@, which is generally not contiguous with the other symbols.
fromTSSymbol :: forall symbol . Symbol symbol => TSSymbol -> symbol
fromTSSymbol symbol = toEnum (min (fromIntegral symbol) (fromEnum (maxBound :: symbol)))


data SymbolType = Regular | Anonymous | Auxiliary
  deriving (Enum, Eq, Lift, Ord, Show)

class (Bounded s, Enum s, Ix s, Ord s, Show s) => Symbol s where
  symbolType :: s -> SymbolType


symbolToName :: SymbolType -> String -> String
symbolToName ty name
  = prefixHidden name
  & toWords
  & filter (not . all (== '_'))
  & map escapeOperatorPunctuation
  & (>>= initUpper)
  & (prefix ++)
  where toWords = split (condense (whenElt (not . isAlpha)))

        prefixHidden s@('_':_) = "Hidden" ++ s
        prefixHidden s         = s

        initUpper (c:cs) = toUpper c : cs
        initUpper ""     = ""

        prefix = case ty of
          Regular   -> ""
          Anonymous -> "Anon"
          Auxiliary -> "Aux"

-- Ensures that we generate valid Haskell identifiers from
-- the literal characters used for infix operators and punctuation.
escapeOperatorPunctuation :: String -> String
escapeOperatorPunctuation = concatMap $ \case
  '{'  -> "LBrace"
  '}'  -> "RBrace"
  '('  -> "LParen"
  ')'  -> "RParen"
  '.'  -> "Dot"
  ':'  -> "Colon"
  ','  -> "Comma"
  '|'  -> "Pipe"
  ';'  -> "Semicolon"
  '*'  -> "Star"
  '&'  -> "Ampersand"
  '='  -> "Equal"
  '<'  -> "LAngle"
  '>'  -> "RAngle"
  '['  -> "LBracket"
  ']'  -> "RBracket"
  '+'  -> "Plus"
  '-'  -> "Minus"
  '/'  -> "Slash"
  '\\' -> "Backslash"
  '^'  -> "Caret"
  '!'  -> "Bang"
  '%'  -> "Percent"
  '@'  -> "At"
  '~'  -> "Tilde"
  '?'  -> "Question"
  '`'  -> "Backtick"
  '#'  -> "Hash"
  '$'  -> "Dollar"
  '"'  -> "DQuote"
  '\'' -> "SQuote"
  '\t' -> "Tab"
  '\n' -> "LF"
  '\r' -> "CR"
  other
    | isControl other -> escapeOperatorPunctuation (show other)
    | otherwise       -> [other]
