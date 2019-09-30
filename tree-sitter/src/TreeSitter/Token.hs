module TreeSitter.Token
( Token(..)
) where

newtype Token sym a = Token { ann :: a }
