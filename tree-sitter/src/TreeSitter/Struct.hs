{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables,TypeOperators , DeriveAnyClass#-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module TreeSitter.Struct (Struct, evalStruct, peekStruct, pokeStruct) where

import Foreign

-- | 'Struct' is a strict 'Monad' with automatic alignment & advancing, & inferred type.
newtype Struct a = Struct { runStruct :: forall b . Ptr b -> IO (a, Ptr a) }

instance Functor Struct where
  fmap f a = Struct go where
    go p = do
      (a', p') <- runStruct a p
      let fa = f a'
      fa `seq` p' `seq` pure (fa, castPtr p')
    {-# INLINE go #-}
  {-# INLINE fmap #-}

instance Applicative Struct where
  pure a = Struct (\ p -> pure (a, castPtr p))
  {-# INLINE pure #-}

  f <*> a = Struct go where
    go p = do
      (f', p')  <- runStruct f          p
      (a', p'') <- p' `seq` runStruct a (castPtr p')
      let fa = f' a'
      fa `seq` p'' `seq` pure (fa, castPtr p'')
    {-# INLINE go #-}
  {-# INLINE (<*>) #-}

instance Monad Struct where
  return = pure
  {-# INLINE return #-}

  a >>= f = Struct go where
    go p = do
      (a', p')   <- runStruct a               p
      (fa', p'') <- p' `seq` runStruct (f a') (castPtr p')
      fa' `seq` p'' `seq` pure (fa', p'')
    {-# INLINE go #-}
  {-# INLINE (>>=) #-}

evalStruct :: Struct a -> Ptr b -> IO a
evalStruct s p = fmap fst $! runStruct s p
{-# INLINE evalStruct #-}

peekStruct :: forall a . Storable a => Struct a
peekStruct = Struct
  ( \p -> do
    let aligned = alignPtr (castPtr p) (alignment (undefined :: a))
    a <- peek aligned
    pure (a, aligned `plusPtr` sizeOf a)
  )
{-# INLINE peekStruct #-}

pokeStruct :: Storable a => a -> Struct ()
pokeStruct a = Struct
  ( \p -> do
    let aligned = alignPtr (castPtr p) (alignment a)
    poke aligned a
    pure ((), castPtr aligned `plusPtr` sizeOf a)
  )
