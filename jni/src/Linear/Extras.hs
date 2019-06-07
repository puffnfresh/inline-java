{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Linear.Extras where

import Control.Monad.Linear
import qualified Prelude
import Prelude.Linear hiding (foldr)

-- | Monadic fold over the elements of a list,
-- associating to the left, i.e. from left to right.
foldM :: forall m a b. Monad m => (b ->. a ->. m b) -> b ->. [a] ->. m b
foldM f z0 xs = foldr f' return xs z0
  where
    f' :: a ->. (b ->. m b) ->. b ->. m b
    f' x k z = f z x >>= k

foldr :: (a ->. b ->. b) -> b ->. [a] ->. b
foldr f z = \case
  [] -> z
  x:xs -> f x (foldr f z xs)

flip :: (a -->.(p) b -->.(q) c) -->.(r) b -->.(q) a -->.(p) c
flip f b a = f a b

(.) :: (b ->. c) ->. (a ->. b) ->. a ->. c
(f . g) x = f (g x)

(<*) :: Applicative f => f a ->. f () ->. f a
fa <* fb = fmap ((\x () -> x) :: a ->. () ->. a) fa <*> fb

(<$) :: forall a f. Applicative f => a ->. f () ->. f a
a <$ fb = pure ((\() -> a) :: () ->. a) <*> fb

withFst :: Functor f => (a ->. f c) ->. (a, b) ->. f (c, b)
withFst f (a, b) = fmap (, b) (f a)

withFst_ :: Functor f => (a ->. f ()) ->. (a, b) ->. f b
withFst_ f (a, b) = fmap (\() -> b) (f a)

withSnd :: Functor f => (b ->. f c) ->. (a, b) ->. f (a, c)
withSnd f (a, b) = fmap (a,) (f b)

uapp :: Unrestricted (a -> b) ->. Unrestricted a ->. Unrestricted b
uapp (Unrestricted f) (Unrestricted a) = Unrestricted (f a)

uliftA2 :: (a -> b -> c) -> Unrestricted a ->. Unrestricted b ->. Unrestricted c
uliftA2 f (Unrestricted a) (Unrestricted b) = Unrestricted (f a b)
