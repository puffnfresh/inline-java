{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unsafe.Linear.Extras where

import qualified Linear.Extras as Linear
import Prelude.Linear
import qualified System.IO
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

withLinear :: a ->. (a -> b) ->. b
withLinear = Linear.flip Unsafe.toLinear

-- | Like 'toLinear' but for three-argument functions
toLinear3 :: (a -> b -> c -> d) ->. (a ->. b ->. c ->. d)
toLinear3 f = Unsafe.toLinear2 (\a b -> Unsafe.toLinear (f a b))

-- TODO: move to Unsafe.Linear
toSystemIO :: Linear.IO a ->. System.IO.IO a
toSystemIO = Unsafe.coerce
