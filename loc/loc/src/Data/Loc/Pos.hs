{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Loc.Pos
  ( Pos
  , Line
  , Column
  , ToNat (..)

  -- * Show and Read
  , posShowsPrec
  , posReadPrec

  ) where

import Data.Loc.Internal.Prelude

import Prelude (Num (..))

{- |

'Pos' stands for /positive integer/. You can also think of it as /position/,
because we use it to represent line and column numbers ('Line' and 'Column').

'Pos' has instances of several of the standard numeric typeclasses, although
many of the operations throw 'Underflow' when non-positive values result.
'Pos' does /not/ have an 'Integral' instance, because there is no sensible
way to implement 'quotRem'.

-}
newtype Pos = Pos Natural
  deriving (Eq, Ord)

instance ToNat Pos
  where

    toNat (Pos n) = n

instance Show Pos
  where

    showsPrec = posShowsPrec

instance Read Pos
  where

    readPrec = posReadPrec

{- |

>>> fromInteger 3 :: Pos
3

>>> fromInteger 0 :: Pos
*** Exception: arithmetic underflow

>>> 2 + 3 :: Pos
5

>>> 3 - 2 :: Pos
1

>>> 3 - 3 :: Pos
*** Exception: arithmetic underflow

>>> 2 * 3 :: Pos
6

>>> negate 3 :: Pos
*** Exception: arithmetic underflow

-}
instance Num Pos
  where

    fromInteger = Pos . checkForUnderflow . fromInteger

    Pos x + Pos y = Pos (x + y)

    Pos x - Pos y = Pos (checkForUnderflow (x - y))

    Pos x * Pos y = Pos (x * y)

    abs = id

    signum _ = Pos 1

    negate _ = throw Underflow

instance Real Pos
  where

    toRational (Pos n) = toRational n

{- |

>>> toEnum 3 :: Pos
3

>>> toEnum 0 :: Pos
*** Exception: arithmetic underflow

>>> fromEnum (3 :: Pos)
3

-}
instance Enum Pos
  where

    toEnum = Pos . checkForUnderflow . toEnum

    fromEnum (Pos n) = fromEnum n

checkForUnderflow :: Natural -> Natural
checkForUnderflow n =
  if n == 0 then throw Underflow else n

{- |

>>> posShowsPrec minPrec 1 ""
"1"

>>> posShowsPrec minPrec 42 ""
"42"

-}
posShowsPrec :: Int -> Pos -> ShowS
posShowsPrec i (Pos n) =
  showsPrec i n

{- |

>>> readPrec_to_S posReadPrec minPrec "1"
[(1,"")]

>>> readPrec_to_S posReadPrec minPrec "42"
[(42,"")]

>>> readPrec_to_S posReadPrec minPrec "0"
[]

>>> readPrec_to_S posReadPrec minPrec "-1"
[]

-}
posReadPrec :: ReadPrec Pos
posReadPrec =
  Pos <$> mfilter (/= 0) readPrec


--------------------------------------------------------------------------------
--  ToNat
--------------------------------------------------------------------------------

{- |

Types that can be converted to 'Natural'.

This class mostly exists so that 'toNat' can be used in situations that would
normally call for 'toInteger' (which we cannot use because 'Pos' does not have
an instance of 'Integral').

-}
class ToNat a
  where

    toNat :: a -> Natural


--------------------------------------------------------------------------------
--  Line
--------------------------------------------------------------------------------

newtype Line = Line Pos
  deriving (Eq, Ord, Num, Real, Enum, ToNat)

instance Show Line
  where

    showsPrec i (Line pos) = showsPrec i pos

instance Read Line
  where

    readPrec = Line <$> readPrec


--------------------------------------------------------------------------------
--  Column
--------------------------------------------------------------------------------

newtype Column = Column Pos
  deriving (Eq, Ord, Num, Real, Enum, ToNat)

instance Show Column
  where

    showsPrec i (Column pos) = showsPrec i pos

instance Read Column
  where

    readPrec = Column <$> readPrec
