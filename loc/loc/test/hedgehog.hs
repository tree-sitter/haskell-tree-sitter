{-# LANGUAGE TemplateHaskell #-}

import Data.Loc
import Data.Loc.Internal.Prelude

import qualified Data.Loc.Area as Area
import qualified Data.Loc.Span as Span

import Hedgehog
import System.IO (hSetEncoding, stdout, stderr, utf8)

import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Loc.Hedgehog.Gen as Gen

main :: IO ()
main =
  hSetEncoding stdout utf8 *>
  hSetEncoding stderr utf8 *>
  checkParallel $$(discover) >>= \ok ->
  when (not ok) exitFailure

prop_Span_add_mempty :: Property
prop_Span_add_mempty =
    property $
    forAll Gen.span' >>= \a ->
    spanArea a + mempty === spanArea a
  where
    (+) = (Area.+)

prop_Span_subtract_mempty :: Property
prop_Span_subtract_mempty =
    property $
    forAll Gen.span' >>= \a ->
    spanArea a - mempty === spanArea a
  where
    (-) = (Area.-)

prop_Area_addition_commutativity :: Property
prop_Area_addition_commutativity =
    property $
    forAll Gen.area' >>= \a ->
    forAll Gen.area' >>= \b ->
    a + b === b + a
  where
    (+) = (Area.+)

prop_Area_add_mempty :: Property
prop_Area_add_mempty =
    property $
    forAll Gen.area' >>= \a ->
    a + mempty === a
  where
    (+) = (Area.+)

prop_Area_subtract_mempty :: Property
prop_Area_subtract_mempty =
    property $
    forAll Gen.area' >>= \a ->
    a - mempty === a
  where
    (-) = (Area.-)

prop_Area_addition_and_subtraction :: Property
prop_Area_addition_and_subtraction =
    property $
    forAll Gen.area' >>= \a ->
    forAll Gen.area' >>= \b ->
    forAll Gen.area' >>= \c ->
    a - b - c === a - (b + c)
  where
    (+) = (Area.+)
    (-) = (Area.-)

prop_Span_joinAsc :: Property
prop_Span_joinAsc =
  property $
  forAll (Gen.list (Range.linear 1 10) Gen.span') >>= \spans ->
  areaSpansAsc (foldMap spanArea spans) === Span.joinAsc (List.sort spans)

prop_Area_addSpan :: Property
prop_Area_addSpan =
  property $
  forAll Gen.area' >>= \a ->
  forAll Gen.span' >>= \s ->
  Area.addSpan s a === areaUnion (spanArea s) a

prop_Area_fromTo_mempty1 :: Property
prop_Area_fromTo_mempty1 =
  property $
  forAll Gen.loc' >>= \x ->
  forAll Gen.loc' >>= \y ->
  (Area.fromTo x y == mempty) === (x == y)

prop_Area_fromTo_mempty2 :: Property
prop_Area_fromTo_mempty2 =
  property $
  forAll Gen.loc' >>= \x ->
  Area.fromTo x x === mempty

prop_Loc_read_show :: Property
prop_Loc_read_show =
  property $
  forAll Gen.loc' >>= \x ->
  read (show x) === x

prop_Span_read_show :: Property
prop_Span_read_show =
  property $
  forAll Gen.span' >>= \x ->
  read (show x) === x

prop_Area_read_show :: Property
prop_Area_read_show =
  property $
  forAll Gen.area' >>= \x ->
  read (show x) === x
