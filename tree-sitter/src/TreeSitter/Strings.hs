module TreeSitter.Strings
  ( camelCase
  , capitalize
  , pascalCase
  ) where

import Data.Char (toUpper)

-- | Convert a snake_case String to camelCase
camelCase :: String -> String
camelCase = foldr appender mempty
  where
    appender :: Char -> String -> String
    appender '_' cs = capitalize cs
    appender c cs   = c : cs

-- | Capitalize a String
capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize []     = []

-- | Convert a snake_case String to PascalCase
pascalCase :: String -> String
pascalCase = capitalize . camelCase
