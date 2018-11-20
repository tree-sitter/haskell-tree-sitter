module Data.Loc.Internal.Prelude
  ( module X
  , (<&>)
  , readPrecChar
  ) where

import Control.Applicative as X (empty, pure, (*>), (<*), (<*>))
import Control.Arrow as X ((<<<), (>>>))
import Control.Exception as X (ArithException (..), Exception, throw)
import Control.Monad as X (Monad (..), guard, mfilter, when)
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.Char (Char)
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), foldMap, traverse_)
import Data.Function as X (const, flip, id, on, ($), (&), (.))
import Data.Functor as X (Functor (..), void, ($>), (<$), (<$>))
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map as X (Map)
import Data.Maybe as X (Maybe (..), catMaybes, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..), Ordering (..), max, min)
import Data.Semigroup as X (Semigroup (..))
import Data.Set as X (Set)
import Data.Traversable as X (mapAccumL, sequenceA, traverse)
import Data.Tuple as X (fst, snd)
import Numeric.Natural as X (Natural)
import Prelude as X (Double, Enum (..), Int, Integral, Real (..), String, div,
                     fromIntegral, print, quotRem, round, sqrt, toInteger,
                     undefined, (/))
import System.Exit as X (exitFailure)
import System.IO as X (IO)
import Text.ParserCombinators.ReadPrec as X (minPrec, readP_to_Prec,
                                             readPrec_to_S)
import Text.Read as X (Read (..), ReadPrec, read)
import Text.Show as X (Show (..), ShowS, showString, shows)

import qualified Text.ParserCombinators.ReadP as ReadP

-- | '<&>' = flip 'fmap'
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | A precedence parser that reads a single specific character.
readPrecChar :: Char -> ReadPrec ()
readPrecChar = void . readP_to_Prec . const . ReadP.char
