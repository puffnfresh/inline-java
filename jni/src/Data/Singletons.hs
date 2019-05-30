{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Singletons where

import GHC.TypeLits (KnownSymbol, Symbol)

data family Sing (ty :: k) :: *

class SingI (ty :: k)  where
  sing :: Sing ty

instance SingI ('[] :: [k]) where
  sing = SNil
instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons sing sing

data instance Sing (n :: Symbol) = KnownSymbol n => SSym

instance KnownSymbol n => SingI n where
  sing = SSym

data instance Sing (a :: [b]) where
  SNil :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

