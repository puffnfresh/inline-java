-- | High-level helper functions for interacting with Java objects, mapping them
-- to Haskell values and vice versa. The 'Reify' and 'Reflect' classes together
-- are to Java what "Foreign.Storable" is to C: they provide a means to
-- marshall/unmarshall Java objects from/to Haskell data types.
--
-- A typical pattern for wrapping Java API's using this module is:
--
-- @
-- {&#45;\# LANGUAGE DataKinds \#&#45;}
-- {&#45;\# LANGUAGE DeriveAnyClass \#&#45;}
-- module Object where
--
-- import Language.Java.Linear as J
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
--   deriving (J.Coercible, J.Interpretation, J.Reify, J.Reflect)
--
-- clone :: Object ->. Linear.IO Object
-- clone obj = J.'call' obj "clone" []
--
-- equals :: Object ->. Object ->. Linear.IO Bool
-- equals obj1 obj2 = J.'call' obj1 "equals" ['jvalue' obj2]
--
-- ...
-- @
--
-- To call Java methods using quasiquoted Java syntax instead, see
-- "Language.Java.Inline.Linear".
--
-- __NOTE 1:__ To use any function in this module, you'll need an initialized
-- JVM in the current process, using 'withJVM' or otherwise.
--
-- __NOTE 2:__ Functions in this module memoize (cache) any implicitly performed
-- class and method lookups, for performance. This memoization is safe only when
-- no new named classes are defined at runtime.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Java.Linear
  ( module Foreign.JNI.Types.Linear
  -- * JVM instance management
  , withJVM
  -- * JVM calls
  , classOf
  , new
  , newArray
  , toArray
  , call
  , callStatic
  , getStaticField
  -- * Coercions
  , Coercible(..)
  , jvalue
  , jobject
  -- * Conversions
  , Interpretation(..)
  , Reify(..)
  , Reflect(..)
  , reify_
  -- * Re-exports
  , sing
  ) where

import Control.Monad.Linear
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Choice as Choice
import qualified Data.Coerce as Coerce
import Data.Int
import Data.Proxy (Proxy(..))
import Data.Singletons (SingI(..))
import Data.Typeable
import Data.Word
import Foreign.C (CChar)
import qualified Foreign.JNI as JNI
import Foreign.JNI.Linear as JNI.Linear
import Foreign.JNI.Types.Linear
import qualified Foreign.JNI.String as JNI
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Language.Java as Java
import Linear.Extras (uliftA2)
import qualified Linear.Extras as Linear
import qualified System.IO.Linear as Linear
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Prelude
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Unsafe.Linear.Extras as Unsafe


-- | A linear variant of "Java.Coercible".
--
-- All types that wrap tracked references can implement
-- have an instance of this class.
class SingI (Ty a) => Coercible a where
  type Ty a :: JType
  coerce :: a ->. JValue
  unsafeUncoerce :: JValue ->. a

  default coerce
    :: Coerce.Coercible a (J (Ty a))
    => a
    ->. JValue
  coerce x = JObject (Unsafe.toLinear Coerce.coerce x :: J (Ty a))

  default unsafeUncoerce
    :: Coerce.Coercible (J (Ty a)) a
    => JValue
    ->. a
  unsafeUncoerce = Unsafe.toLinear $ \case
    JObject obj -> Coerce.coerce (unsafeCast obj :: J (Ty a))
    _ ->
      error "Cannot unsafeUncoerce: object expected but value of primitive type found."

instance SingI ty => Coercible (J ty) where
  type Ty (J ty) = ty

withTypeRep :: Typeable a => (TypeRep -> a) -> a
withTypeRep f = let x = f (typeOf x) in x

coercePrim :: Java.Coercible a => a ->. JValue
coercePrim x = JValue (Unsafe.toLinear Java.coerce x)

unsafeUncoercePrim :: (Typeable a, Java.Coercible a) => JValue ->. a
unsafeUncoercePrim = Unsafe.toLinear $ \case
    JValue v -> Java.unsafeUncoerce v
    val -> withTypeRep
      (\r -> error ("unsafeUncoercePrim can't uncoerce a reference: "
                      ++ show (val, r)
                   )
      )

instance Coercible Bool where
  type Ty Bool = Java.Ty Bool
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible CChar where
  type Ty CChar = Java.Ty CChar
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Char where
  type Ty Char = Java.Ty Char
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Word16 where
  type Ty Word16 = Java.Ty Word16
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Int16 where
  type Ty Int16 = Java.Ty Int16
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Int32 where
  type Ty Int32 = Java.Ty Int32
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Int64 where
  type Ty Int64 = Java.Ty Int64
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Float where
  type Ty Float = Java.Ty Float
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Double where
  type Ty Double = Java.Ty Double
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible () where
  type Ty () = Java.Ty ()
  coerce = error "Void value undefined."
  unsafeUncoerce = Unsafe.toLinear (const ())
instance Coercible (Choice.Choice a) where
  type Ty (Choice.Choice a) = Java.Ty Bool
  coerce c = coerce (Unsafe.toLinear Choice.toBool c)
  unsafeUncoerce v = Unsafe.toLinear Choice.fromBool (unsafeUncoerce v)

instance (Java.Coercible a, Typeable a) => Coercible (Unrestricted a) where
  type Ty (Unrestricted a) = Java.Ty a
  coerce (Unrestricted a) = JValue (Java.coerce a)
  unsafeUncoerce v = Unsafe.toLinear Unrestricted (unsafeUncoercePrim v)

-- | Get the Java class of an object or anything 'Coercible' to one.
classOf
  :: forall a sym. (Ty a ~ 'Class sym, Coercible a, KnownSymbol sym)
  => a
  ->. (a, JNI.String)
classOf = Unsafe.toLinear $ \x -> (,) x $
  JNI.fromChars (symbolVal (Proxy :: Proxy sym)) `const` coerce x

-- | Creates a new instance of the class whose name is resolved from the return
-- type. For instance,
--
-- @
-- do x :: 'J' (''Class' "java.lang.Integer") <- new ['coerce' 42]
--    return x
-- @
new
  :: forall a sym.
     (Ty a ~ 'Class sym, Coercible a)
  => [JValue]
  ->. Linear.IO a
new = Unsafe.toLinear $ \args -> fmap unsafeUncoerce $ Linear.fromSystemIO $
    JObject . J <$> Java.newJ @sym (toJNIJValues args)
      Prelude.<* deleteLinearJObjects args

-- | Creates a new Java array of the given size. The type of the elements
-- of the resulting array is determined by the return type a call to
-- 'newArray' has, at the call site, and must not be left ambiguous.
--
-- To create a Java array of 50 booleans:
--
-- @
-- do arr :: 'J' (''Array' (''Prim' "boolean")) <- 'newArray' 50
--    return arr
-- @
newArray :: forall ty. SingI ty => Int32 -> Linear.IO (J ('Array ty))
newArray sz = Linear.fromSystemIO $ J <$> Java.newArray sz

-- | Creates an array from a list of references.
toArray
  :: forall ty. (SingI ty, IsReferenceType ty)
  => [J ty]
  ->. Linear.IO ([J ty], J ('Array ty))
toArray = Unsafe.toLinear $ \xs ->
  Linear.fromSystemIO $ (,) xs . J <$> Java.toArray (Coerce.coerce xs)

-- | The Swiss Army knife for calling Java methods. Give it an object or
-- any data type coercible to one, the name of a method, and a list of
-- arguments. Based on the type indexes of each argument, and based on the
-- return type, 'call' will invoke the named method using of the @call*Method@
-- family of functions in the JNI API.
--
-- When the method name is overloaded, use 'upcast' or 'unsafeCast'
-- appropriately on the class instance and/or on the arguments to invoke the
-- right method.
call
  :: forall a b ty1 ty2.
     ( ty1 ~ Ty a
     , ty2 ~ Ty b
     , IsReferenceType ty1
     , Coercible a
     , Coercible b
     , Coerce.Coercible a (J ty1)
     )
  => a -- ^ Any object or value 'Coercible' to one
  ->. JNI.String -- ^ Method name
  ->. [JValue] -- ^ Arguments
  ->. Linear.IO b
call = Unsafe.toLinear3 $ \obj mname args ->
    fmap unsafeUncoerce $ Linear.fromSystemIO $ do
      fromJNIJValue <$>
        Java.callToJValue @ty1
          (sing :: Sing ty1) (Coerce.coerce obj) mname (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

fromJNIJValue :: Java.JValue -> JValue
fromJNIJValue = \case
    Java.JObject j -> JObject (J j)
    v -> JValue v

-- | Same as 'call', but for static methods.
callStatic
  :: forall a ty. (ty ~ Ty a, Coercible a)
  => JNI.String -- ^ Class name
  ->. JNI.String -- ^ Method name
  ->. [JValue] -- ^ Arguments
  ->. Linear.IO a
callStatic = Unsafe.toLinear3 $ \cname mname args ->
    fmap unsafeUncoerce $ Linear.fromSystemIO $ fromJNIJValue <$>
      Java.callStaticToJValue (sing :: Sing ty) cname mname (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

-- | Get a static field.
getStaticField
  :: forall a ty. (ty ~ Ty a, Coercible a)
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Static field name
  -> Linear.IO a
getStaticField cname fname =
    fmap unsafeUncoerce $ Linear.fromSystemIO $
      fromJNIJValue <$>
        Java.getStaticFieldAsJValue (sing :: Sing ty) cname fname

-- | Inject a value (of primitive or reference type) to a 'JValue'. This
-- datatype is useful for e.g. passing arguments as a list of homogeneous type.
-- Synonym for 'coerce'.
jvalue :: (ty ~ Ty a, Coercible a) => a ->. JValue
jvalue = coerce

-- | If @ty@ is a reference type, then it should be possible to get an object
-- from a value.
jobject :: (ty ~ Ty a, Coercible a, IsReferenceType ty) => a ->. J ty
jobject = Unsafe.toLinear $ \x ->
  case coerce x of
    JObject jobj -> unsafeCast jobj
    _ -> error "impossible"

-- | The 'Interp' type family is used by both 'Reify' and 'Reflect'. In order to
-- benefit from @-XGeneralizedNewtypeDeriving@ of new instances, we make this an
-- /associated/ type family instead of a standalone one.
class (SingI (Interp a), IsReferenceType (Interp a)) => Interpretation (a :: k) where
  -- | Map a Haskell type to the symbolic representation of a Java type.
  type Interp a :: JType

-- | Extract a concrete Haskell value from the space of Java objects. That is to
-- say, unmarshall a Java object to a Haskell value. Unlike coercing, in general
-- reifying induces allocations and copies.
class Interpretation a => Reify a where
  -- | Invariant: The result and the argument share no direct JVM object
  -- references.
  reify :: J (Interp a) ->. Linear.IO (J (Interp a), Unrestricted a)

  default reify
    :: (Java.Coercible a, Interp a ~ Java.Ty a)
    => J (Interp a)
    ->. Linear.IO (J (Interp a), Unrestricted a)
  reify = Unsafe.toLinear $ \x -> fmap ((,) x)
      (Linear.fromSystemIOU
        (Java.unsafeUncoerce . Java.JObject <$> JNI.newLocalRef (unJ x))
      )

reify_ :: Reify a => J (Interp a) ->. Linear.IO (Unrestricted a)
reify_ _j = reify _j >>= \(_j, a) -> a Linear.<$ deleteLocalRef _j

-- | Inject a concrete Haskell value into the space of Java objects. That is to
-- say, marshall a Haskell value to a Java object. Unlike coercing, in general
-- reflection induces allocations and copies.
class Interpretation a => Reflect a where
  -- | Invariant: The result and the argument share no direct JVM object
  -- references.
  reflect :: a -> Linear.IO (J (Interp a))

  default reflect
    :: (Java.Coercible a, Interp a ~ Java.Ty a)
    => a
    -> Linear.IO (J (Interp a))
  reflect x = Linear.fromSystemIO (J <$> JNI.newLocalRef (Java.jobject x))

instance (SingI ty, IsReferenceType ty) => Interpretation (Java.J ty) where type Interp (Java.J ty) = ty
instance Interpretation (Java.J ty) => Reify (Java.J ty)
instance Interpretation (Java.J ty) => Reflect (Java.J ty)

-- Ugly work around the fact that java has no equivalent of the 'unit' type:
-- We take an arbitrary serializable type to represent it.
instance Interpretation () where type Interp () = 'Class "java.lang.Short"
instance Reify () where
  reify x = return (x, Unrestricted ())
instance Reflect () where
  reflect () = new [JValue (Java.JShort 0)]

instance Interpretation ByteString where
  type Interp ByteString = 'Array ('Prim "byte")

instance Reify ByteString where
  reify jobj =
      getArrayLength (unsafeCast jobj) >>= \(jobj1, Unrestricted n) ->
      getByteArrayElements jobj1 >>= \(jobj2, Unrestricted bytes) ->
      Linear.fromSystemIOU (BS.packCStringLen (bytes, fromIntegral n)) >>= \bs ->
      fmap (, bs) (releaseByteArrayElements jobj2 bytes)

instance Reflect ByteString where
  reflect bs = Linear.fromSystemIO
      (BS.unsafeUseAsCStringLen bs (\(content, n) ->
        Unsafe.toSystemIO
          (newByteArray (fromIntegral n) >>= \arr ->
           setByteArrayRegion arr 0 (fromIntegral n) content
          )
      ))

instance Interpretation Bool where
  type Interp Bool = 'Class "java.lang.Boolean"

instance Reify Bool where
  reify jobj =
      let method = unsafeDupablePerformIO
                     Prelude.$ Linear.withLinearIO
                     Prelude.$
            findClass (referenceTypeName (SClass "java.lang.Boolean"))
              >>= \(Unrestricted klass) ->
            getMethodID klass "booleanValue"
                   (methodSignature [] (SPrim "boolean"))
            Linear.<* Linear.fromSystemIO (JNI.deleteLocalRef klass)
       in callBooleanMethod jobj method []

instance Reflect Bool where
  reflect x = new [JValue (Java.JBoolean (fromIntegral (fromEnum x)))]

instance Interpretation CChar where
  type Interp CChar = 'Class "java.lang.Byte"

instance Reify CChar where
  reify jobj = do
      let method = unsafeDupablePerformIO
                     Prelude.$ Linear.withLinearIO
                     Prelude.$
            findClass (referenceTypeName (SClass "java.lang.Byte"))
              >>= \(Unrestricted klass) ->
            getMethodID klass "byteValue"
                   (methodSignature [] (SPrim "byte"))
            Linear.<*
            Linear.fromSystemIO (JNI.deleteLocalRef klass)
       in callByteMethod jobj method []

instance Reflect CChar where
  reflect x = new [JValue (Java.JByte x)]

{-
  instance Interpretation Int16 where
    type Interp Int16 = 'Class "java.lang.Short"

  instance Reify Int16 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Short"))
              m <- getMethodID klass "shortValue"
                     (methodSignature [] (SPrim "short"))
              deleteLocalRef klass
              return m
        callShortMethod jobj method []

  instance Reflect Int16 where
    reflect x = new [JShort x]

  instance Interpretation Int32 where
    type Interp Int32 = 'Class "java.lang.Integer"

  instance Reify Int32 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass
                         (referenceTypeName (SClass "java.lang.Integer"))
              m <- getMethodID klass "intValue"
                     (methodSignature [] (SPrim "int"))
              deleteLocalRef klass
              return m
        callIntMethod jobj method []

  instance Reflect Int32 where
    reflect x = new [JInt x]

  instance Interpretation Int64 where
    type Interp Int64 = 'Class "java.lang.Long"

  instance Reify Int64 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Long"))
              m <- getMethodID klass "longValue"
                     (methodSignature [] (SPrim "long"))
              deleteLocalRef klass
              return m
        callLongMethod jobj method []

  instance Reflect Int64 where
    reflect x = new [JLong x]

  instance Interpretation Word16 where
    type Interp Word16 = 'Class "java.lang.Character"

  instance Reify Word16 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass
                         (referenceTypeName (SClass "java.lang.Character"))
              m <- getMethodID klass "charValue"
                     (methodSignature [] (SPrim "char"))
              deleteLocalRef klass
              return m
        fromIntegral <$> callCharMethod jobj method []

  instance Reflect Word16 where
    reflect x = new [JChar x]

  instance Interpretation Double where
    type Interp Double = 'Class "java.lang.Double"

  instance Reify Double where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Double"))
              m <- getMethodID klass "doubleValue"
                     (methodSignature [] (SPrim "double"))
              deleteLocalRef klass
              return m
        callDoubleMethod jobj method []

  instance Reflect Double where
    reflect x = new [JDouble x]

  instance Interpretation Float where
    type Interp Float = 'Class "java.lang.Float"

  instance Reify Float where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Float"))
              m <- getMethodID klass "floatValue"
                     (methodSignature [] (SPrim "float"))
              deleteLocalRef klass
              return m
        callFloatMethod jobj method []

  instance Reflect Float where
    reflect x = new [JFloat x]

  instance Interpretation Text where
    type Interp Text = 'Class "java.lang.String"

  instance Reify Text where
    reify jobj = do
        sz <- getStringLength jobj
        cs <- getStringChars jobj
        txt <- Text.fromPtr cs (fromIntegral sz)
        releaseStringChars jobj cs
        return txt

  instance Reflect Text where
    reflect x =
        Text.useAsPtr x $ \ptr len ->
          newString ptr (fromIntegral len)

-- Instances can't be compiled on GHC 8.0.1 due to
-- https://ghc.haskell.org/trac/ghc/ticket/12082.
#if ! (__GLASGOW_HASKELL__ == 800 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1)
  instance Interpretation (IOVector Word16) where
    type Interp (IOVector Word16) = 'Array ('Prim "char")

  instance Reify (IOVector Word16) where
    reify = reifyMVector getCharArrayElements releaseCharArrayElements

  instance Reflect (IOVector Word16) where
    reflect = reflectMVector newCharArray setCharArrayRegion

  instance Interpretation (IOVector Int16) where
    type Interp (IOVector Int16) = 'Array ('Prim "short")

  instance Reify (IOVector Int16) where
    reify = reifyMVector getShortArrayElements releaseShortArrayElements

  instance Reflect (IOVector Int16) where
    reflect = reflectMVector newShortArray setShortArrayRegion

  instance Interpretation (IOVector Int32) where
    type Interp (IOVector Int32) = 'Array ('Prim "int")

  instance Reify (IOVector Int32) where
    reify = reifyMVector (getIntArrayElements) (releaseIntArrayElements)

  instance Reflect (IOVector Int32) where
    reflect = reflectMVector (newIntArray) (setIntArrayRegion)

  instance Interpretation (IOVector Int64) where
    type Interp (IOVector Int64) = 'Array ('Prim "long")

  instance Reify (IOVector Int64) where
    reify = reifyMVector getLongArrayElements releaseLongArrayElements

  instance Reflect (IOVector Int64) where
    reflect = reflectMVector newLongArray setLongArrayRegion

  instance Interpretation (IOVector Float) where
    type Interp (IOVector Float) = 'Array ('Prim "float")

  instance Reify (IOVector Float) where
    reify = reifyMVector getFloatArrayElements releaseFloatArrayElements

  instance Reflect (IOVector Float) where
    reflect = reflectMVector newFloatArray setFloatArrayRegion

  instance Interpretation (IOVector Double) where
    type Interp (IOVector Double) = 'Array ('Prim "double")

  instance Reify (IOVector Double) where
    reify = reifyMVector (getDoubleArrayElements) (releaseDoubleArrayElements)

  instance Reflect (IOVector Double) where
    reflect = reflectMVector (newDoubleArray) (setDoubleArrayRegion)

  instance Interpretation (IOVector a) => Interpretation (Vector a) where
    type Interp (Vector a) = Interp (IOVector a)

  instance (Storable a, Reify (IOVector a)) => Reify (Vector a) where
    reify = Vector.freeze <=< reify

  instance (Storable a, Reflect (IOVector a)) => Reflect (Vector a) where
    reflect = reflect <=< Vector.thaw
#endif
-}

instance Interpretation a => Interpretation [a] where
  type Interp [a] = 'Array (Interp a)

instance Reify a => Reify [a] where
  reify _jobj =
      getArrayLength _jobj >>= \(_jobj, Unrestricted n) ->
      Linear.foldM
        (\(_jobj, uxs) i ->
          getObjectArrayElement _jobj i >>= \(_jobj, jx) ->
          reify_ jx >>= \ux ->
          return (_jobj, uliftA2 (:) ux uxs)
        )
        (_jobj, Unrestricted []) [n-1,n-2..0]

instance Reflect a => Reflect [a] where
  reflect xs =
      let n = fromIntegral (length xs)
       in newArray n >>= \array ->
      Linear.foldM
        (\array0 (Unrestricted (i, x)) ->
            reflect x >>= \jx ->
            setObjectArrayElement_ array0 i jx
        )
        array (map Unrestricted (zip [0..n-1] xs))
