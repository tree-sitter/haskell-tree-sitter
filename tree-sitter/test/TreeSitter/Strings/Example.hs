{-# LANGUAGE TemplateHaskell #-}
module TreeSitter.Strings.Example (tests) where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           TreeSitter.Symbol

tests :: IO Bool
tests = checkParallel $$(discover)

-- | Generate permutations of alphabet and underscore combinations
snakeChar :: MonadGen m => m Char
snakeChar = Gen.choice
  [ pure '_'
  , Gen.alpha
  ]

-- | Generator for snake_case JSON input
genSnake :: Gen String
genSnake = Gen.string (Range.constant 1 10) snakeChar

prop_camelCase :: Property
prop_camelCase = property $ do
  xs <- forAll $ Gen.string (Range.constant 1 5) snakeChar
  assert ('_' `notElem` camelCase xs)

initialCaps :: MonadGen m => m Char
initialCaps = Gen.frequency
  [ (10, Gen.upper)
  , (5, pure '_')
  , (1, Gen.lower)
  ]

prop_capitalize :: Property
prop_capitalize = property $ do
  x:xs <- forAll (Gen.string (Range.constant 1 5) initialCaps)
  y:_  <- pure $ capitalize (x:xs)
  when (isLower x) (assert (isUpper y))

prop_escapePunct :: Property
prop_escapePunct = property $ do
  xs <- forAll $ Gen.string (Range.constant 1 5) (Gen.filter p Gen.ascii)
  traverse_ (assert . isAlphaNum) (escapeOperatorPunctuation xs)
  where p = not . (\c -> isSpace c || c == '_' )
