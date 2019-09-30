{-# LANGUAGE DeriveLift, LambdaCase #-}
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
fromTSSymbol :: Symbol symbol => TSSymbol -> symbol
fromTSSymbol symbol
  | symbol >= fromIntegral (fromEnum err) = err
  | otherwise                             = toEnum (fromIntegral symbol)
  where err = parseErrorSymbol

-- | Map a value of a 'Symbol' datatype to the corresponding 'TSSymbol'.
--
--   This should be used instead of 'fromEnum' to perform this conversion, because tree-sitter represents parse errors with the unsigned short @65535@, which is generally not contiguous with the other symbols.
toTSSymbol :: Symbol symbol => symbol -> TSSymbol
toTSSymbol symbol
  | symbol == parseErrorSymbol = maxBound
  | otherwise                  = fromIntegral (fromEnum symbol)

-- | The value of a 'Symbol' datatype representing parse errors.
parseErrorSymbol :: Symbol symbol => symbol
parseErrorSymbol = maxBound


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
