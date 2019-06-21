{-# LANGUAGE DeriveLift, ScopedTypeVariables #-}
module TreeSitter.Symbol where

import Data.Char (isAlpha, toUpper)
import Data.Function ((&))
import Data.Ix (Ix)
import Data.List.Split (condense, split, whenElt)
import Data.Word (Word16)
import Language.Haskell.TH.Syntax

type TSSymbol = Word16

-- | Map a 'TSSymbol' to the corresponding value of a 'Symbol' datatype.
--
--   This should be used instead of 'toEnum' to perform this conversion, because tree-sitter represents parse errors with the unsigned short @65535@, which is generally not contiguous with the other symbols.
toSymbol :: forall symbol . Symbol symbol => TSSymbol -> symbol
toSymbol symbol = toEnum (min (fromIntegral symbol) (fromEnum (maxBound :: symbol)))


data SymbolType = Regular | Anonymous | Auxiliary
  deriving (Enum, Eq, Lift, Ord, Show)

class (Bounded s, Enum s, Ix s, Ord s, Show s) => Symbol s where
  symbolType :: s -> SymbolType


symbolToName :: SymbolType -> String -> String
symbolToName ty name
  = prefixHidden name
  & toWords
  & filter (not . all (== '_'))
  & map (>>= toDescription)
  & (>>= initUpper)
  & (prefix ++)
  where toWords = split (condense (whenElt (not . isAlpha)))

        prefixHidden s@('_':_) = "Hidden" ++ s
        prefixHidden s         = s

        initUpper (c:cs) = toUpper c : cs
        initUpper ""     = ""

        toDescription '{'  = "LBrace"
        toDescription '}'  = "RBrace"
        toDescription '('  = "LParen"
        toDescription ')'  = "RParen"
        toDescription '.'  = "Dot"
        toDescription ':'  = "Colon"
        toDescription ','  = "Comma"
        toDescription '|'  = "Pipe"
        toDescription ';'  = "Semicolon"
        toDescription '*'  = "Star"
        toDescription '&'  = "Ampersand"
        toDescription '='  = "Equal"
        toDescription '<'  = "LAngle"
        toDescription '>'  = "RAngle"
        toDescription '['  = "LBracket"
        toDescription ']'  = "RBracket"
        toDescription '+'  = "Plus"
        toDescription '-'  = "Minus"
        toDescription '/'  = "Slash"
        toDescription '\\' = "Backslash"
        toDescription '^'  = "Caret"
        toDescription '!'  = "Bang"
        toDescription '%'  = "Percent"
        toDescription '@'  = "At"
        toDescription '~'  = "Tilde"
        toDescription '?'  = "Question"
        toDescription '`'  = "Backtick"
        toDescription '#'  = "Hash"
        toDescription '$'  = "Dollar"
        toDescription '"'  = "DQuote"
        toDescription '\'' = "SQuote"
        toDescription '\t' = "Tab"
        toDescription '\n' = "LF"
        toDescription '\r' = "CR"
        toDescription c    = [c]

        prefix = case ty of
          Regular   -> ""
          Anonymous -> "Anon"
          Auxiliary -> "Aux"
