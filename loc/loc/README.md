# loc

Overview of the concepts:

![Example text illustrating Loc, Span, and Area](https://raw.githubusercontent.com/chris-martin/haskell-libraries/4be81df645d4a2e5073f45563930e202e41209c7/loc/example.png)

* `Loc` - a cursor position, starting at the origin `1:1`
* `Span` - a nonempty contiguous region between two locs
* `Area` - a set of zero or more spans with gaps between them

See also:

* [loc-test](https://hackage.haskell.org/package/loc-test) -
  Test-related utilities for this package.

## `Pos`

Since all of the numbers we're dealing with in this domain are positive, we
define a "positive integer" type. This is a newtype for `Natural` that doesn't
allow zero.

```haskell
newtype Pos = Pos Natural
  deriving (Eq, Ord)

instance Num Pos where
  fromInteger = Pos . checkForUnderflow . fromInteger
  Pos x + Pos y = Pos (x + y)
  Pos x - Pos y = Pos (checkForUnderflow (x - y))
  Pos x * Pos y = Pos (x * y)
  abs = id
  signum _ = Pos 1
  negate _ = throw Underflow

checkForUnderflow :: Natural -> Natural
checkForUnderflow n =
  if n == 0 then throw Underflow else n
```

`Pos` does not have an `Integral` instance, because that would require
implementing `quotRem :: Pos -> Pos -> (Pos, Pos)`, which doesn't make much
sense. Therefore we can't use `toInteger` on `Pos`. Instead we use our own
`ToNat` class to convert positive numbers to natural numbers.

```haskell
class ToNat a where
  toNat :: a -> Natural

instance ToNat Pos where
  toNat (Pos n) = n
```

## `Line`, `Column`

We then add some newtypes to be more specific about whether we're talking about
line or column numbers.

```haskell
newtype Line = Line Pos
  deriving (Eq, Ord, Num, Real, Enum, ToNat)

newtype Column = Column Pos
  deriving (Eq, Ord, Num, Real, Enum, ToNat)
```

## `Loc`

A `Loc` is a `Line` and a `Column`.

```haskell
data Loc = Loc
  { line   :: Line
  , column :: Column
  }
  deriving (Eq, Ord)
```

Note that this library has chosen to be remain entirely agnostic of the text
that the positions are referring to. Therefore there is no "plus one" operation
on `Loc`, because the next `Loc` after *4:17* could be either *4:18* or *5:1* -
we can't tell without knowing the line lengths.

## `Span`

A `Span` is a start `Loc` and an end `Loc`.

```haskell
data Span = Span
  { start :: Loc
  , end   :: Loc
  } deriving (Eq, Ord)
```

A `Span` is not allowed to be empty; in other words, `start` and `end` must be
different.

There are two functions for constructing a `Span`. They both reorder their
arguments as appropriate to make sure the start comes before the end (so that
spans are never backwards). They take different approaches to ensuring that
spans are never empty: the first can throw an exception, whereas the second is
typed as `Maybe`.

```haskell
fromTo :: Loc -> Loc -> Span
fromTo a b =
  maybe (throw EmptySpan) id (fromToMay a b)

fromToMay :: Loc -> Loc -> Maybe Span
fromToMay a b =
  case compare a b of
    LT -> Just (Span a b)
    GT -> Just (Span b a)
    EQ -> Nothing
```

The choice to use an exclusive upper bound *\[start, end)* rather than two
inclusive bounds *\[start, end\]* is forced by the decision to be text-agnostic.
With inclusive ranges, you couldn't tell whether span *4:16-4:17* abuts span
*5:1-5:2* without knowing whether the character at position *4:17* is a newline.

## `Area`

Conceptually, an area is a set of spans. To support efficient union and
difference operations, `Area` is defined like this:

```haskell
data Terminus = Start | End
  deriving (Eq, Ord)

newtype Area = Area (Map Loc Terminus)
  deriving (Eq, Ord)
```

You can think of this as a sorted list of the spans' start and end positions,
along with a tag indicating whether each is a start or an end.

## `Show`

We define custom `Show` and `Read` instances to be able to write terse
[doctests](https://hackage.haskell.org/package/doctest) like

```haskell
>>> addSpan (read "1:1-6:1") (read "[1:1-3:1,6:1-6:2,7:4-7:5]")
[1:1-6:2,7:4-7:5]
```

These are the `showsPrec` implementations for `Loc` and `Span`:

```haskell
locShowsPrec :: Int -> Loc -> ShowS
locShowsPrec _ (Loc l c) =
  shows l .
  showString ":" .
  shows c

spanShowsPrec :: Int -> Span -> ShowS
spanShowsPrec _ (Span a b) =
  locShowsPrec 10 a .
  showString "-" .
  locShowsPrec 10 b
```

## `Read`

The parser for `Pos` is based on the parser for `Natural`, applying `mfilter (/=
0)` to make the parser fail if the input represents a zero.

```haskell
posReadPrec :: ReadPrec Pos
posReadPrec =
  Pos <$> mfilter (/= 0) readPrec
```

As a reminder, the type of `mfilter` is:

```haskell
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
```

The `Loc` parser uses a very typical `Applicative` pattern:

```haskell
-- | Parses a single specific character.
readPrecChar :: Char -> ReadPrec ()
readPrecChar = void . readP_to_Prec . const . ReadP.char

locReadPrec :: ReadPrec Loc
locReadPrec =
  Loc              <$>
  readPrec         <*
  readPrecChar ':' <*>
  readPrec
```

We used `mfilter` above to introduce failure into the `Pos` parser; for `Span`
we use `empty`.

```haskell
empty :: Alternative f => f a
```

First we use `fromToMay` to produce a `Maybe Span`, and then in the case where
the result is `Nothing` we use `empty` to make the parser fail.

```haskell
spanReadPrec :: ReadPrec Span
spanReadPrec =
  locReadPrec      >>= \a ->
  readPrecChar '-' *>
  locReadPrec      >>= \b ->
  maybe empty pure (fromToMay a b)
```

## Comparison to similar packages

### `srcloc`

[srcloc](https://hackage.haskell.org/package/srcloc) has a similar general
purpose: defining types related to positions in text files.

Some differences:

* `srcloc`'s `Pos` type (comparable to our `Loc` type) has a `FilePath`
  parameter, whereas this library doesn't consider file paths at all.
* `srcloc` has nothing comparable to the `Area` type.

There are some undocumented aspects of `srcloc` we find confusing:

* What does "character offset" mean?
* Does `srcloc`'s `Loc` type use inclusive or exclusive bounds?
