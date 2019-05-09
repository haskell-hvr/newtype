{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DefaultSignatures      #-}
#endif

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |
Module : Control.Newtype
SPDX-License-Identifier : BSD-3-Clause

The 'Newtype' typeclass and related functions: 'op', 'ala', 'ala'', 'under'. Primarly pulled from Conor McBride's Epigram work. Some examples:

>>> ala Sum foldMap [1,2,3,4] -- foldMaps the list ala the Sum newtype
10

>>> ala Product foldMap [1,2,3,4] -- foldMaps the list ala the Product newtype
24

>>> ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3 -- foldMaps the list ala the Endo newtype
8

NB: 'Data.Foldable.foldMap' is a generalized @mconcatMap@ which is a generalized 'concatMap'.

This package includes 'Newtype' instances for all the (non-GHC\/foreign) newtypes in base (as seen in the examples). However, there are neat things you can do with this with /any/ @newtype@ and you should definitely define your own 'Newtype' instances for the power of this library. For example, see \"@'ala' Cont 'traverse'@\", with the proper 'Newtype' instance for @Cont@.

-}
module Control.Newtype
    ( Newtype(..)
    , op
    , ala
    , ala'
    , under
    , over
    , underF
    , overF
    ) where

import           Control.Applicative
import           Control.Arrow
import           Data.Fixed
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Ord

#if !MIN_VERSION_base(4,7,0)
-- necessary evil for 'Newtype Fixed'
import           Unsafe.Coerce         (unsafeCoerce)
#else
import           Data.Coerce
#endif

-- | Given a @newtype@ @n@, we will always have the same unwrapped type @o@, meaning we can represent this with a fundep @n -> o@.
--
-- Any instance of this class just needs to let 'pack' equal to the newtype's constructor, and let 'unpack' destruct the @newtype@ with pattern matching.
--
-- Starting with @base-4.7.0.0@ / GHC 7.8 default method implementations are provided using "Data.Coerce", i.e.:
--
-- @
-- 'pack' = 'coerce'
-- 'unpack' = 'coerce'
-- @
--
-- When omitting the method definitions with GHC 7.4 and 7.6 a compile error will be triggered.
class Newtype n o | n -> o where
  pack :: o -> n

  unpack :: n -> o

#if  __GLASGOW_HASKELL__ >= 704
  default pack :: Coercible o n => o -> n
  pack = coerce

  default unpack :: Coercible n o => n -> o
  unpack = coerce
#endif

#if __GLASGOW_HASKELL__ >= 704 && !MIN_VERSION_base(4,7,0)
-- Hack: We define a dummy 'Coercible' class to force a
-- missing-instance compile error when methods are not explicitly
-- defined
class Coercible o n

coerce :: a -> b
coerce = undefined
#endif

{- TODO: for newtype-0.3

This would be nice, but it breaks in odd ways with GHC < 7.
Oh, and it also makes the instances an extra line longer. :(
class Newtype n where
  type Orig n
  pack :: Orig n -> n
  unpack :: n -> Orig n
-}

-- |
-- This function serves two purposes:
--
-- 1. Giving you the unpack of a @newtype@ without you needing to remember the name.
--
-- 2. Showing that the first parameter is /completely ignored/ on the value level, meaning the only reason you pass in the constructor is to provide type information. Typeclasses sure are neat.
op :: Newtype n o => (o -> n) -> n -> o
op _ = unpack

-- | The workhorse of the package. Given a pack and a \"higher order function\", it handles the packing and unpacking, and just sends you back a regular old function, with the type varying based on the /hof/ (higher-order function) you passed.
--
-- The reason for the signature of the hof is due to 'ala' not caring about structure. To illustrate why this is important, another function in this package is 'under'. It is not extremely useful; @under2@ might be more useful (with e.g., 'mappend'), but then we already digging the trench of \"What about @under3@? @under4@?\". The solution utilized here is to just hand off the \"packer\" to the hof. That way your structure can be imposed in the hof, whatever you may want it to be (e.g., List, Traversable).
ala :: (Newtype n o, Newtype n' o') => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

-- | This is the original function seen in Conor McBride's work. The way it differs from the 'ala' function in this package, is that it provides an extra hook into the \"packer\" passed to the hof. However, this normally ends up being 'id', so 'ala' wraps this function and passes 'id' as the final parameter by default. If you want the convenience of being able to hook right into the hof, you may use this function.
ala' :: (Newtype n o, Newtype n' o') => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
ala' _ hof f = unpack . hof (pack . f)

-- | A very simple operation involving running the function \"under\" the newtype. Suffers from the problems mentioned in the 'ala' function's documentation.
under :: (Newtype n o, Newtype n' o') => (o -> n) -> (n -> n') -> (o -> o')
under _ f = unpack . f . pack

-- | The opposite of 'under'. I.e., take a function which works on the underlying types, and switch it to a function that works on the newtypes.
--
-- @since 0.2
over :: (Newtype n o, Newtype n' o') => (o -> n) -> (o -> o') -> (n -> n')
over _ f = pack . f . unpack

-- | 'under' lifted into a 'Functor'.
--
-- @since 0.2
underF :: (Newtype n o, Newtype n' o', Functor f) => (o -> n) -> (f n -> f n') -> (f o -> f o')
underF _ f = fmap unpack . f . fmap pack

-- | 'over' lifted into a 'Functor'.
--
-- @since 0.2
overF :: (Newtype n o, Newtype n' o', Functor f) => (o -> n) -> (f o -> f o') -> (f n -> f n')
overF _ f = fmap pack . f . fmap unpack

instance Newtype All Bool where
  pack = All
  unpack (All a) = a

instance Newtype Any Bool where
  pack = Any
  unpack (Any a) = a

instance Newtype (Sum a) a where
  pack = Sum
  unpack (Sum a) = a

instance Newtype (Product a) a where
  pack = Product
  unpack (Product a) = a

-- | @since 0.2
instance Newtype (Kleisli m a b) (a -> m b) where
  pack = Kleisli
  unpack (Kleisli a) = a

instance Newtype (WrappedMonad m a) (m a) where
  pack = WrapMonad
  unpack (WrapMonad a) = a

instance Newtype (WrappedArrow a b c) (a b c) where
  pack = WrapArrow
  unpack (WrapArrow a) = a

instance Newtype (ZipList a) [a] where
  pack = ZipList
  unpack (ZipList a) = a

instance Newtype (Const a x) a where
  pack = Const
  unpack (Const a) = a

instance Newtype (Endo a) (a -> a) where
  pack = Endo
  unpack (Endo a) = a

instance Newtype (First a) (Maybe a) where
  pack = First
  unpack (First a) = a

instance Newtype (Last a) (Maybe a) where
  pack = Last
  unpack (Last a) = a

instance ArrowApply a => Newtype (ArrowMonad a b) (a () b) where
  pack = ArrowMonad
  unpack (ArrowMonad a) = a

-- | @since 0.2.1.0
instance Newtype (Fixed a) Integer where
#if MIN_VERSION_base(4,7,0)
  pack = MkFixed
  unpack (MkFixed x) = x
#else
  -- 'Fixed' is a newtype, but its constructor wasn't exported before base-4.7.0
  pack = unsafeCoerce
  unpack = unsafeCoerce
#endif

-- | @since 0.2.1.0
instance Newtype (Dual a) a where
  pack = Dual
  unpack (Dual a) = a

#if MIN_VERSION_base(4,8,0)
-- | __NOTE__: Type & instance only available with @base ≥ 4.8.0@
--
-- @since 0.2.1.0
instance Newtype (Alt f a) (f a) where
  pack = Alt
  unpack (Alt x) = x
#endif

#if MIN_VERSION_base(4,6,0)
-- | __NOTE__: Type & instance only available with @base ≥ 4.6.0@
--
-- @since 0.2.1.0
instance Newtype (Down a) a where
  pack = Down
  unpack (Down a) = a
#endif

-- | @since 0.2.1.0
instance Newtype (Identity a) a where
  pack = Identity
  unpack (Identity a) = a

-- | @since 0.2.1.0
instance Newtype (Compose f g a) (f (g a)) where
  pack = Compose
  unpack (Compose x) = x

#if MIN_VERSION_base(4,12,0)
-- | __NOTE__: Type & instance only available with @base ≥ 4.12.0@
--
-- @since 0.2.1.0
instance Newtype (Ap f a) (f a) where
  pack = Ap
  unpack (Ap x) = x
#endif

-- TODO
--  Data.Semigroup
--  Data.Functor.Contravariant
