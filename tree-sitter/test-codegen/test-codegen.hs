{-# LANGUAGE TemplateHaskell, TypeApplications #-}

module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Bool (bool)
import Data.Char
import TreeSitter.GenerateSyntax
import Data.Foldable
import Control.Monad
import System.Exit (exitFailure, exitSuccess)
import TreeSitter.Symbol

-- | Generate permutations of alphabet and underscore combinations
snakeChar :: MonadGen m => m Char
snakeChar = Gen.choice
  [ pure '_'
  , Gen.alpha
  ]

-- | Generator for snake_case JSON input
genSnake :: Gen String
genSnake = Gen.string (Range.constant 1 10) snakeChar

prop_removeUnderscore :: Property
prop_removeUnderscore = property $ do
  xs <- forAll $ Gen.string (Range.constant 1 5) snakeChar
  assert ('_' `notElem` removeUnderscore xs)

initialCaps :: MonadGen m => m Char
initialCaps = Gen.frequency
  [ (10, Gen.upper)
  , (5, pure '_')
  , (1, Gen.lower)
  ]

prop_initUpper :: Property
prop_initUpper = property $ do
  (x:xs) <- forAll (Gen.string (Range.constant 1 5) initialCaps)
  (y:ys) <- pure $ initUpper (x:xs)
  when (isLower x) (assert (isUpper y))

prop_escapePunct :: Property
prop_escapePunct = property $ do
  xs <- forAll $ Gen.string (Range.constant 1 5) (Gen.filter p Gen.ascii)
  traverse_ (assert . isAlphaNum) (escapeOperatorPunctuation xs)
  where p = not . (\c -> isSpace c || c == '_' )

main :: IO ()
main = checkParallel $$(discover) >>= bool exitFailure exitSuccess
