-- | Low-level bindings to the Java Native Interface (JNI).
--
-- Read the
-- <https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html JNI spec>
-- for authoritative documentation as to what each of the functions in
-- this module does. The names of the bindings in this module were chosen to
-- match the names of the functions in the JNI spec.
--
-- All bindings in this module access the JNI via a thread-local variable of
-- type @JNIEnv *@. If the current OS thread has not yet been "attached" to the
-- JVM, it needs to be attached. See 'JNI.runInAttachedThread'.
--
-- The 'String' type in this module is the type of JNI strings. See
-- "Foreign.JNI.String".
--
-- The functions in this module are considered unsafe in opposition
-- to those in "Foreign.JNI.Safe", which ensure that local references are not
-- leaked.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- XXX This file uses cpphs for preprocessing instead of the system's native
-- CPP, because the OS X has subtly different whitespace behaviour in the
-- presence of concatenation.

module Foreign.JNI.Unsafe
  ( -- * JNI functions
    -- ** VM management
    withJVM
  , newJVM
  , destroyJVM
    -- ** Class loading
  , defineClass
  , JNINativeMethod(..)
  , registerNatives
    -- ** String wrappers
  , ReferenceTypeName
  , MethodSignature
  , Signature
    -- ** Exceptions
  , JVMException(..)
  , throw
  , throwNew
    -- ** Query functions
  , findClass
  , getFieldID
  , getStaticFieldID
  , getMethodID
  , getStaticMethodID
  , getObjectClass
    -- ** Reference manipulation
  , newGlobalRef
  , deleteGlobalRef
  , newGlobalRefNonFinalized
  , deleteGlobalRefNonFinalized
  , newLocalRef
  , deleteLocalRef
  , pushLocalFrame
  , popLocalFrame
    -- ** Field accessor functions
    -- *** Get fields
  , getObjectField
  , getBooleanField
  , getIntField
  , getLongField
  , getCharField
  , getShortField
  , getByteField
  , getDoubleField
  , getFloatField
    -- *** Get static fields
  , getStaticObjectField
  , getStaticBooleanField
  , getStaticIntField
  , getStaticLongField
  , getStaticCharField
  , getStaticShortField
  , getStaticByteField
  , getStaticDoubleField
  , getStaticFloatField
    -- *** Set fields
  , setObjectField
  , setBooleanField
  , setIntField
  , setLongField
  , setCharField
  , setShortField
  , setByteField
  , setDoubleField
  , setFloatField
    -- *** Set static fields
  , setStaticObjectField
  , setStaticBooleanField
  , setStaticIntField
  , setStaticLongField
  , setStaticCharField
  , setStaticShortField
  , setStaticByteField
  , setStaticDoubleField
  , setStaticFloatField
    -- ** Method invocation
  , callObjectMethod
  , callBooleanMethod
  , callIntMethod
  , callLongMethod
  , callCharMethod
  , callShortMethod
  , callByteMethod
  , callDoubleMethod
  , callFloatMethod
  , callVoidMethod
  , callStaticObjectMethod
  , callStaticVoidMethod
  , callStaticBooleanMethod
  , callStaticIntMethod
  , callStaticLongMethod
  , callStaticCharMethod
  , callStaticShortMethod
  , callStaticByteMethod
  , callStaticDoubleMethod
  , callStaticFloatMethod
    -- ** Object construction
  , newObject
  , newString
  , newObjectArray
  , newBooleanArray
  , newByteArray
  , newCharArray
  , newShortArray
  , newIntArray
  , newLongArray
  , newFloatArray
  , newDoubleArray
    -- ** Array manipulation
  , getArrayLength
  , getStringLength
  , ArrayCopyFailed(..)
  , NullPointerException(..)
  , getBooleanArrayElements
  , getByteArrayElements
  , getCharArrayElements
  , getShortArrayElements
  , getIntArrayElements
  , getLongArrayElements
  , getFloatArrayElements
  , getDoubleArrayElements
  , getStringChars
  , getBooleanArrayRegion
  , getByteArrayRegion
  , getCharArrayRegion
  , getShortArrayRegion
  , getIntArrayRegion
  , getLongArrayRegion
  , getFloatArrayRegion
  , getDoubleArrayRegion
  , setBooleanArrayRegion
  , setByteArrayRegion
  , setCharArrayRegion
  , setShortArrayRegion
  , setIntArrayRegion
  , setLongArrayRegion
  , setFloatArrayRegion
  , setDoubleArrayRegion
  , releaseBooleanArrayElements
  , releaseByteArrayElements
  , releaseCharArrayElements
  , releaseShortArrayElements
  , releaseIntArrayElements
  , releaseLongArrayElements
  , releaseFloatArrayElements
  , releaseDoubleArrayElements
  , releaseStringChars
  , getObjectArrayElement
  , setObjectArrayElement
    -- * Thread management
  , attachCurrentThreadAsDaemon
  , detachCurrentThread
  , runInAttachedThread
  , ThreadNotAttached(..)
    -- * NIO support
  , DirectBufferFailed(..)
  , newDirectByteBuffer
  , getDirectBufferAddress
  , getDirectBufferCapacity
  ) where

import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, bracket, bracket_, catch, finally, throwIO)
import Control.Monad (join, unless, void, when)
import Data.Choice
import Data.Coerce
import Data.Int
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Foreign.C (CChar)
import Foreign.ForeignPtr
  ( finalizeForeignPtr
  , newForeignPtr_
  , withForeignPtr
  )
import Foreign.JNI.Internal
import Foreign.JNI.NativeMethod
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import GHC.ForeignPtr (newConcForeignPtr)
import GHC.Stack (HasCallStack, callStack, getCallStack, prettySrcLoc)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO (fixIO)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (String)
import qualified Prelude

C.context (C.baseCtx <> C.bsCtx <> jniCtx)

C.include "<jni.h>"
C.include "<stdio.h>"
C.include "<errno.h>"
C.include "<stdlib.h>"

-- A thread-local variable to cache the JNI environment. Accessing this variable
-- is faster than calling @jvm->GetEnv()@.
$(C.verbatim "static __thread JNIEnv* jniEnv; ")

-- | A JNI call may cause a (Java) exception to be raised. This module raises it
-- as a Haskell exception wrapping the Java exception.
newtype JVMException = JVMException JThrowable
  deriving (Show, Typeable)

instance Exception JVMException

-- | Thrown when @Get<PrimitiveType>ArrayElements@ returns a null pointer,
-- because it wanted to copy the array contents but couldn't. In this case the
-- JVM doesn't throw OutOfMemory according to the JNI spec.
data ArrayCopyFailed = ArrayCopyFailed
  deriving (Exception, Show, Typeable)

-- Thrown when @NewDirectByteBuffer@ or @GetDirectBufferAddress@ returns NULL,
-- and when @GetDirectBufferCapacity@ return @-1@.
data DirectBufferFailed = DirectBufferFailed
  deriving (Exception, Show, Typeable)

-- | A null reference is found where a non-null reference was expected.
data NullPointerException = NullPointerException
  deriving (Exception, Show, Typeable)

-- | A JNI call is made from a thread not attached to the JVM.
data ThreadNotAttached = ThreadNotAttached
  deriving (Exception, Show, Typeable)

-- | A JNI call is made from an unbound thread.
data ThreadNotBound = ThreadNotBound
  deriving (Exception, Show, Typeable)

-- | Thrown when an JNI call is made from an unbound thread.
data JNIError = JNIError Prelude.String Int32
  deriving (Show, Typeable)

instance Exception JNIError

-- | Map Java exceptions to Haskell exceptions.
throwIfException :: Ptr JNIEnv -> IO a -> IO a
throwIfException env m = m `finally` do
    excptr <- [CU.exp| jthrowable { (*$(JNIEnv *env))->ExceptionOccurred($(JNIEnv *env)) } |]
    unless (excptr == nullPtr) $ do
      [CU.exp| void { (*$(JNIEnv *env))->ExceptionDescribe($(JNIEnv *env)) } |]
      [CU.exp| void { (*$(JNIEnv *env))->ExceptionClear($(JNIEnv *env)) } |]
      throwIO . JVMException =<< newGlobalRef =<< objectFromPtr excptr

-- | Check whether a pointer is null.
throwIfNull :: Exception e => e -> IO (Ptr a) -> IO (Ptr a)
throwIfNull e m = do
    ptr <- m
    if ptr == nullPtr
    then throwIO e
    else return ptr

-- | Throws an error if the given reference is null, otherwise performs
-- the given io action.
throwIfJNull :: J ty -> IO a -> IO a
throwIfJNull j io =
    if j == jnull
    then throwIO NullPointerException
    else io

-- | A read-write lock
--
-- Concurrent readers are allowed, but only one writer is supported.
newtype RWLock =
    RWLock (IORef (Int, RWWantedState))
    -- ^ A count of the held read locks and the wanted state

-- | The wanted state of the RW
data RWWantedState
    = Reading            -- ^ There are no writers
    | Writing (MVar ())
       -- ^ A writer wants to write, grant no more read locks. The MVar is used
       -- to notify the writer when the currently held read locks are released.

-- | Creates a new read-write lock.
newRWLock :: IO RWLock
newRWLock = RWLock <$> newIORef (0, Reading)

-- | Tries to acquire a read lock. If this call returns `Do #read`, no writer
-- will be granted a lock before the read lock is released.
tryAcquireReadLock :: RWLock -> IO (Choice "read")
tryAcquireReadLock (RWLock ref) = do
    atomicModifyIORef ref $ \case
      (!readers,  Reading) -> ((readers + 1, Reading),    Do #read)
      st                   -> (                       st, Don't #read)

-- | Releases a read lock.
releaseReadLock :: RWLock -> IO ()
releaseReadLock (RWLock ref) = do
    st <- atomicModifyIORef ref $
            \st@(readers, aim) -> ((readers - 1, aim), st)
    case st of
      -- Notify the writer if I'm the last reader.
      (1, Writing mv) -> putMVar mv ()
      _               -> return ()

-- | Waits until the current read locks are released and grants a write lock.
acquireWriteLock :: RWLock -> IO ()
acquireWriteLock (RWLock ref) = do
    mv <- newEmptyMVar
    join $ atomicModifyIORef ref $ \(readers, _) ->
      ((readers, Writing mv), when (readers > 0) (takeMVar mv))

-- | This lock is used to avoid the JVM from dying before any finalizers
-- deleting global references are finished.
--
-- Finalizers try to acquire read locks.
--
-- The JVM acquires a write lock before shutdown. Thence, finalizers fail to
-- acquire read locks and behave as noops.
globalJVMLock :: RWLock
globalJVMLock = unsafePerformIO newRWLock
{-# NOINLINE globalJVMLock #-}

throwIfNotOK_ :: HasCallStack => IO Int32 -> IO ()
throwIfNotOK_ m = m >>= \case
  rc
    | rc == [CU.pure| jint { JNI_OK } |] -> return ()
    | rc == [CU.pure| jint { JNI_EDETACHED } |] -> throwIO ThreadNotAttached
    | otherwise -> throwIO $ JNIError (prettySrcLoc loc) rc
  where
    (_, loc):_ = getCallStack callStack

attachCurrentThreadAsDaemon :: IO ()
attachCurrentThreadAsDaemon = do
    throwIfNotOK_
      [CU.exp| jint {
        (*$(JavaVM* jvm))->AttachCurrentThreadAsDaemon($(JavaVM* jvm), (void**)&jniEnv, NULL)
      } |]

detachCurrentThread :: IO ()
detachCurrentThread =
    throwIfNotOK_
    [CU.block| jint {
      int rc = (*$(JavaVM* jvm))->DetachCurrentThread($(JavaVM* jvm));
      if (rc == JNI_OK)
        jniEnv = NULL;
      return rc;
    } |]

-- | Attaches the calling thread to the JVM, runs the given IO action and
-- then detaches the thread.
--
-- If the thread is already attached no attaching and detaching is performed.
runInAttachedThread :: IO a -> IO a
runInAttachedThread io = do
    attached <-
      catch (getJNIEnv >> return True) (\ThreadNotAttached -> return False)
    if attached
    then io
    else bracket_
          attachCurrentThreadAsDaemon
          detachCurrentThread
          io

-- | The current JVM
--
-- Assumes there's at most one JVM. The current JNI spec (2016) says only
-- one JVM per process is supported anyways.
{-# NOINLINE jvm #-}
jvm :: Ptr JVM
jvm = unsafePerformIO $ alloca $ \pjvm -> alloca $ \pnum_jvms -> do
    throwIfNotOK_
      [CU.exp| jint {
        JNI_GetCreatedJavaVMs($(JavaVM** pjvm), 1, $(jsize* pnum_jvms))
      }|]
    num_jvms <- peek pnum_jvms
    when (num_jvms == 0) $
      fail "JNI_GetCreatedJavaVMs: No JVM has been initialized yet."
    when (num_jvms > 1) $
      fail "JNI_GetCreatedJavaVMs: There are multiple JVMs but only one is supported."
    peek pjvm

-- | Yields the JNIEnv of the calling thread.
--
-- Yields @Nothing@ if the calling thread is not attached to the JVM.
getJNIEnv :: IO (Ptr JNIEnv)
getJNIEnv = [CU.exp| JNIEnv* { jniEnv } |] >>= \case
    env | env == nullPtr -> do
      throwIfNotOK_
        [CU.exp| jint {
          (*$(JavaVM* jvm))->GetEnv($(JavaVM* jvm), (void**)&jniEnv, JNI_VERSION_1_6)
        }|]
      [CU.exp| JNIEnv* { jniEnv } |]
    env -> return env

-- | Run an action against the appropriate 'JNIEnv'.
--
-- Each OS thread has its own 'JNIEnv', which this function gives access to.
withJNIEnv :: (Ptr JNIEnv -> IO a) -> IO a
withJNIEnv f = getJNIEnv >>= f

useAsCStrings :: [ByteString] -> ([Ptr CChar] -> IO a) -> IO a
useAsCStrings strs m =
  foldr (\str k cstrs -> BS.useAsCString str $ \cstr -> k (cstr:cstrs)) m strs []

-- | Create a new JVM, with the given arguments. /Can only be called once/. Best
-- practice: use 'withJVM' instead. Only useful for GHCi.
newJVM :: [ByteString] -> IO JVM
newJVM options = JVM_ <$> do
    useAsCStrings options $ \cstrs -> do
      withArray cstrs $ \(coptions :: Ptr (Ptr CChar)) -> do
        let n = fromIntegral (length cstrs) :: C.CInt
        checkBoundness
        [C.block| JavaVM * {
          JavaVM *jvm;
          JavaVMInitArgs vm_args;
          JavaVMOption *options = malloc(sizeof(JavaVMOption) * $(int n));
          for(int i = 0; i < $(int n); i++)
                  options[i].optionString = $(char **coptions)[i];
          vm_args.version = JNI_VERSION_1_6;
          vm_args.nOptions = $(int n);
          vm_args.options = options;
          vm_args.ignoreUnrecognized = 0;
          JNI_CreateJavaVM(&jvm, (void**)&jniEnv, &vm_args);
          free(options);
          return jvm; } |]

  where
    checkBoundness :: IO ()
    checkBoundness = when rtsSupportsBoundThreads $ do
      bound <- isCurrentThreadBound
      unless bound (throwIO ThreadNotBound)

-- | Deallocate a 'JVM' created using 'newJVM'.
destroyJVM :: JVM -> IO ()
destroyJVM (JVM_ jvm) = do
    acquireWriteLock globalJVMLock
    [C.block| void {
        (*$(JavaVM *jvm))->DestroyJavaVM($(JavaVM *jvm));
        jniEnv = NULL;
    } |]

-- | Create a new JVM, with the given arguments. Destroy it once the given
-- action completes. /Can only be called once/. Best practice: use it to wrap
-- your @main@ function.
withJVM :: [ByteString] -> IO a -> IO a
withJVM options action = bracket (newJVM options) destroyJVM (const action)

defineClass
  :: Coercible o (J ('Class "java.lang.ClassLoader"))
  => ReferenceTypeName -- ^ Class name
  -> o -- ^ Loader
  -> ByteString -- ^ Bytecode buffer
  -> IO JClass
defineClass (coerce -> name) (coerce -> upcast -> loader) buf = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString name $ \namep ->
    objectFromPtr =<<
    [CU.exp| jclass {
      (*$(JNIEnv *env))->DefineClass($(JNIEnv *env),
                                     $(char *namep),
                                     $fptr-ptr:(jobject loader),
                                     $bs-ptr:buf,
                                     $bs-len:buf) } |]
registerNatives
  :: JClass
  -> [JNINativeMethod]
  -> IO ()
registerNatives cls methods = withJNIEnv $ \env ->
    throwIfException env $
    withArray methods $ \cmethods -> do
      let numMethods = fromIntegral $ length methods
      _ <- [CU.exp| jint {
             (*$(JNIEnv *env))->RegisterNatives($(JNIEnv *env),
                                                $fptr-ptr:(jclass cls),
                                                $(JNINativeMethod *cmethods),
                                                $(int numMethods)) } |]
      return ()

throw :: Coercible o (J a) => o -> IO ()
throw (coerce -> upcast -> obj) = withJNIEnv $ \env -> void $ do
    [CU.exp| jint {
       (*$(JNIEnv *env))->Throw($(JNIEnv *env),
                                $fptr-ptr:(jobject obj)) } |]

throwNew :: JClass -> JNI.String -> IO ()
throwNew cls msg = throwIfJNull cls $ withJNIEnv $ \env ->
    JNI.withString msg $ \msgp -> void $ do
    [CU.exp| jint {
       (*$(JNIEnv *env))->ThrowNew($(JNIEnv *env),
                                   $fptr-ptr:(jclass cls),
                                   $(char *msgp)) } |]

findClass
  :: ReferenceTypeName -- ^ Class name
  -> IO JClass
findClass (coerce -> name) = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString name $ \namep ->
    objectFromPtr =<<
    [CU.exp| jclass { (*$(JNIEnv *env))->FindClass($(JNIEnv *env), $(char *namep)) } |]

newObject :: JClass -> MethodSignature -> [JValue] -> IO JObject
newObject cls (coerce -> sig) args = throwIfJNull cls $ withJNIEnv $ \env ->
    throwIfException env $
    withJValues args $ \cargs -> do
      constr <- getMethodID cls "<init>" sig
      objectFromPtr =<< [CU.exp| jobject {
        (*$(JNIEnv *env))->NewObjectA($(JNIEnv *env),
                                      $fptr-ptr:(jclass cls),
                                      $(jmethodID constr),
                                      $(jvalue *cargs)) } |]

getFieldID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> Signature -- ^ JNI signature
  -> IO JFieldID
getFieldID cls fieldname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString fieldname $ \fieldnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jfieldID {
      (*$(JNIEnv *env))->GetFieldID($(JNIEnv *env),
                                    $fptr-ptr:(jclass cls),
                                    $(char *fieldnamep),
                                    $(char *sigp)) } |]

getStaticFieldID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> Signature -- ^ JNI signature
  -> IO JFieldID
getStaticFieldID cls fieldname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString fieldname $ \fieldnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jfieldID {
      (*$(JNIEnv *env))->GetStaticFieldID($(JNIEnv *env),
                                          $fptr-ptr:(jclass cls),
                                          $(char *fieldnamep),
                                          $(char *sigp)) } |]

#define GET_FIELD(name, hs_rettype, c_rettype) \
get/**/name/**/Field :: Coercible o (J a) => o -> JFieldID -> IO hs_rettype; \
get/**/name/**/Field (coerce -> upcast -> obj) field = withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| c_rettype { \
      (*$(JNIEnv *env))->Get/**/name/**/Field($(JNIEnv *env), \
                                              $fptr-ptr:(jobject obj), \
                                              $(jfieldID field)) } |]

getObjectField :: Coercible o (J a) => o -> JFieldID -> IO JObject
getObjectField x y =
    let GET_FIELD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< getObjectField x y
GET_FIELD(Boolean, Word8, jboolean)
GET_FIELD(Byte, CChar, jbyte)
GET_FIELD(Char, Word16, jchar)
GET_FIELD(Short, Int16, jshort)
GET_FIELD(Int, Int32, jint)
GET_FIELD(Long, Int64, jlong)
GET_FIELD(Float, Float, jfloat)
GET_FIELD(Double, Double, jdouble)

#define GET_STATIC_FIELD(name, hs_rettype, c_rettype) \
getStatic/**/name/**/Field :: JClass -> JFieldID -> IO hs_rettype; \
getStatic/**/name/**/Field klass field = throwIfJNull klass $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| c_rettype { \
      (*$(JNIEnv *env))->GetStatic/**/name/**/Field($(JNIEnv *env), \
                                                    $fptr-ptr:(jclass klass), \
                                                    $(jfieldID field)) } |]

getStaticObjectField :: JClass -> JFieldID -> IO JObject
getStaticObjectField x y =
    let GET_STATIC_FIELD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< getStaticObjectField x y
GET_STATIC_FIELD(Boolean, Word8, jboolean)
GET_STATIC_FIELD(Byte, CChar, jbyte)
GET_STATIC_FIELD(Char, Word16, jchar)
GET_STATIC_FIELD(Short, Int16, jshort)
GET_STATIC_FIELD(Int, Int32, jint)
GET_STATIC_FIELD(Long, Int64, jlong)
GET_STATIC_FIELD(Float, Float, jfloat)
GET_STATIC_FIELD(Double, Double, jdouble)

#define SET_FIELD(name, hs_fieldtype, c_fieldtype) \
set/**/name/**/Field :: Coercible o (J a) => o -> JFieldID -> hs_fieldtype -> IO (); \
set/**/name/**/Field (coerce -> upcast -> obj) field x = \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.block| void { \
      (*$(JNIEnv *env))->Set/**/name/**/Field($(JNIEnv *env), \
                                              $fptr-ptr:(jobject obj), \
                                              $(jfieldID field), \
                                              $(c_fieldtype x)); } |]

setObjectField :: Coercible o (J a) => o -> JFieldID -> JObject -> IO ()
setObjectField x y z =
    let SET_FIELD(Object, (Ptr JObject), jobject)
    in withForeignPtr (coerce z) (setObjectField x y)
SET_FIELD(Boolean, Word8, jboolean)
SET_FIELD(Byte, CChar, jbyte)
SET_FIELD(Char, Word16, jchar)
SET_FIELD(Short, Int16, jshort)
SET_FIELD(Int, Int32, jint)
SET_FIELD(Long, Int64, jlong)
SET_FIELD(Float, Float, jfloat)
SET_FIELD(Double, Double, jdouble)

#define SET_STATIC_FIELD(name, hs_fieldtype, c_fieldtype) \
setStatic/**/name/**/Field :: JClass -> JFieldID -> hs_fieldtype -> IO (); \
setStatic/**/name/**/Field klass field x = throwIfJNull klass $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.block| void { \
      (*$(JNIEnv *env))->SetStatic/**/name/**/Field($(JNIEnv *env), \
                                                    $fptr-ptr:(jclass klass), \
                                                    $(jfieldID field), \
                                                    $(c_fieldtype x)); } |]

setStaticObjectField :: JClass -> JFieldID -> JObject -> IO ()
setStaticObjectField x y z =
    let SET_STATIC_FIELD(Object, (Ptr JObject), jobject)
    in withForeignPtr (coerce z) (setStaticObjectField x y)
SET_STATIC_FIELD(Boolean, Word8, jboolean)
SET_STATIC_FIELD(Byte, CChar, jbyte)
SET_STATIC_FIELD(Char, Word16, jchar)
SET_STATIC_FIELD(Short, Int16, jshort)
SET_STATIC_FIELD(Int, Int32, jint)
SET_STATIC_FIELD(Long, Int64, jlong)
SET_STATIC_FIELD(Float, Float, jfloat)
SET_STATIC_FIELD(Double, Double, jdouble)

getMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> MethodSignature -- ^ JNI signature
  -> IO JMethodID
getMethodID cls methodname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString methodname $ \methodnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jmethodID {
      (*$(JNIEnv *env))->GetMethodID($(JNIEnv *env),
                                     $fptr-ptr:(jclass cls),
                                     $(char *methodnamep),
                                     $(char *sigp)) } |]

getStaticMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> MethodSignature -- ^ JNI signature
  -> IO JMethodID
getStaticMethodID cls methodname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString methodname $ \methodnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jmethodID {
      (*$(JNIEnv *env))->GetStaticMethodID($(JNIEnv *env),
                                           $fptr-ptr:(jclass cls),
                                           $(char *methodnamep),
                                           $(char *sigp)) } |]

getObjectClass :: Coercible o (J ty) => o -> IO JClass
getObjectClass (coerce -> upcast -> obj) = throwIfJNull obj $
    withJNIEnv $ \env ->
    objectFromPtr =<<
    [CU.exp| jclass {
      (*$(JNIEnv *env))->GetObjectClass($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]

-- | Creates a global reference to the object referred to by
-- the given reference.
--
-- Arranges for a finalizer to call 'deleteGlobalRef' when the
-- global reference is no longer reachable on the Haskell side.
newGlobalRef :: Coercible o (J ty) => o -> IO o
newGlobalRef (coerce -> upcast -> obj) = withJNIEnv $ \env -> do
    gobj <-
      [CU.exp| jobject {
        (*$(JNIEnv *env))->NewGlobalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]
    fixIO $ \j ->
      coerce <$> J <$> newConcForeignPtr gobj (deleteGlobalRefNonFinalized j)

deleteGlobalRef :: Coercible o (J ty) => o -> IO ()
deleteGlobalRef (coerce -> J p) = finalizeForeignPtr p

-- | Like 'newGlobalRef' but it doesn't attach a finalizer to destroy
-- the reference when it is not longer reachable. Use
-- 'deleteGlobalRefNonFinalized' to destroy this reference.
newGlobalRefNonFinalized :: Coercible o (J ty) => o -> IO o
newGlobalRefNonFinalized (coerce -> upcast -> obj) = withJNIEnv $ \env -> do
    gobj <-
      [CU.exp| jobject {
        (*$(JNIEnv *env))->NewGlobalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]
    coerce <$> J <$> newForeignPtr_ gobj

-- | Like 'deleteGlobalRef' but it can be used only on references created with
-- 'newGlobalRefNonFinalized'.
deleteGlobalRefNonFinalized :: Coercible o (J ty) => o -> IO ()
deleteGlobalRefNonFinalized (coerce -> upcast -> obj) = do
    bracket (tryAcquireReadLock globalJVMLock)
            (\doRead -> when (toBool doRead) $ releaseReadLock globalJVMLock)
            $ \doRead ->
      when (toBool doRead) $ withJNIEnv $ \env -> do
        [CU.block| void { (*$(JNIEnv *env))->DeleteGlobalRef($(JNIEnv *env)
                                                            ,$fptr-ptr:(jobject obj));
                        } |]

-- NB: Cannot add a finalizer to local references because it may
-- run in a thread where the reference is not valid.
newLocalRef :: Coercible o (J ty) => o -> IO o
newLocalRef (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    coerce <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->NewLocalRef($(JNIEnv *env),
                                     $fptr-ptr:(jobject obj)) } |]

deleteLocalRef :: Coercible o (J ty) => o -> IO ()
deleteLocalRef (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->DeleteLocalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]

pushLocalFrame :: Int32 -> IO ()
pushLocalFrame (coerce -> capacity) = withJNIEnv $ \env ->
    -- We ignore the output as it is always 0 on success and throws an
    -- exception otherwise.
    throwIfException env $ void $
    [CU.block| jint {
      (*$(JNIEnv *env))->PushLocalFrame($(JNIEnv *env),
                                        $(jint capacity)); } |]

popLocalFrame :: Coercible o (J ty) => o -> IO o
popLocalFrame (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    coerce <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->PopLocalFrame($(JNIEnv *env),
                                       $fptr-ptr:(jobject obj)) } |]

-- Modern CPP does have ## for concatenating strings, but we use the hacky /**/
-- comment syntax for string concatenation. This is because GHC passes
-- the -traditional flag to the preprocessor by default, which turns off several
-- modern CPP features.

#define CALL_METHOD(name, hs_rettype, c_rettype) \
call/**/name/**/Method :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO hs_rettype; \
call/**/name/**/Method (coerce -> upcast -> obj) method args = withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->Call/**/name/**/MethodA($(JNIEnv *env), \
                                                 $fptr-ptr:(jobject obj), \
                                                 $(jmethodID method), \
                                                 $(jvalue *cargs)) } |]

CALL_METHOD(Void, (), void)
callObjectMethod :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO JObject
callObjectMethod x y z =
    let CALL_METHOD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< callObjectMethod x y z
callBooleanMethod :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO Bool
callBooleanMethod x y z =
    let CALL_METHOD(Boolean, Word8, jboolean)
    in toEnum . fromIntegral <$> callBooleanMethod x y z
CALL_METHOD(Byte, CChar, jbyte)
CALL_METHOD(Char, Word16, jchar)
CALL_METHOD(Short, Int16, jshort)
CALL_METHOD(Int, Int32, jint)
CALL_METHOD(Long, Int64, jlong)
CALL_METHOD(Float, Float, jfloat)
CALL_METHOD(Double, Double, jdouble)

#define CALL_STATIC_METHOD(name, hs_rettype, c_rettype) \
callStatic/**/name/**/Method :: JClass -> JMethodID -> [JValue] -> IO hs_rettype; \
callStatic/**/name/**/Method cls method args = throwIfJNull cls $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->CallStatic/**/name/**/MethodA($(JNIEnv *env), \
                                                       $fptr-ptr:(jclass cls), \
                                                       $(jmethodID method), \
                                                       $(jvalue *cargs)) } |]

CALL_STATIC_METHOD(Void, (), void)
callStaticObjectMethod :: JClass -> JMethodID -> [JValue] -> IO JObject
callStaticObjectMethod x y z =
    let CALL_STATIC_METHOD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< callStaticObjectMethod x y z
callStaticBooleanMethod :: JClass -> JMethodID -> [JValue] -> IO Bool
callStaticBooleanMethod x y z =
    let CALL_STATIC_METHOD(Boolean, Word8, jboolean)
    in toEnum . fromIntegral <$> callStaticBooleanMethod x y z
CALL_STATIC_METHOD(Byte, CChar, jbyte)
CALL_STATIC_METHOD(Char, Word16, jchar)
CALL_STATIC_METHOD(Short, Int16, jshort)
CALL_STATIC_METHOD(Int, Int32, jint)
CALL_STATIC_METHOD(Long, Int64, jlong)
CALL_STATIC_METHOD(Float, Float, jfloat)
CALL_STATIC_METHOD(Double, Double, jdouble)

newObjectArray :: Int32 -> JClass -> IO JObjectArray
newObjectArray sz cls = throwIfJNull cls $ withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jobjectArray {
      (*$(JNIEnv *env))->NewObjectArray($(JNIEnv *env),
                                        $(jsize sz),
                                        $fptr-ptr:(jclass cls),
                                        NULL) } |]

#define NEW_ARRAY(name, c_rettype) \
new/**/name/**/Array :: Int32 -> IO J/**/name/**/Array; \
new/**/name/**/Array sz = withJNIEnv $ \env -> \
    throwIfException env $ \
    objectFromPtr =<< \
    [CU.exp| c_rettype/**/Array { \
      (*$(JNIEnv *env))->New/**/name/**/Array($(JNIEnv *env), \
                                              $(jsize sz)) } |]

NEW_ARRAY(Boolean, jboolean)
NEW_ARRAY(Byte, jbyte)
NEW_ARRAY(Char, jchar)
NEW_ARRAY(Short, jshort)
NEW_ARRAY(Int, jint)
NEW_ARRAY(Long, jlong)
NEW_ARRAY(Float, jfloat)
NEW_ARRAY(Double, jdouble)

newString :: Ptr Word16 -> Int32 -> IO JString
newString ptr len = withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jstring {
      (*$(JNIEnv *env))->NewString($(JNIEnv *env),
                                   $(jchar *ptr),
                                   $(jsize len)) } |]

getArrayLength :: Coercible o (JArray a) => o -> IO Int32
getArrayLength (coerce -> upcast -> array) = throwIfJNull array $
    withJNIEnv $ \env ->
    [C.exp| jsize {
      (*$(JNIEnv *env))->GetArrayLength($(JNIEnv *env),
                                        $fptr-ptr:(jarray array)) } |]

getStringLength :: JString -> IO Int32
getStringLength jstr = throwIfJNull jstr $ withJNIEnv $ \env ->
    [CU.exp| jsize {
      (*$(JNIEnv *env))->GetStringLength($(JNIEnv *env),
                                         $fptr-ptr:(jstring jstr)) } |]

#define GET_ARRAY_ELEMENTS(name, hs_rettype, c_rettype) \
get/**/name/**/ArrayElements :: J/**/name/**/Array -> IO (Ptr hs_rettype); \
get/**/name/**/ArrayElements (upcast -> array) = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfNull ArrayCopyFailed $ \
    [CU.exp| c_rettype* { \
      (*$(JNIEnv *env))->Get/**/name/**/ArrayElements($(JNIEnv *env), \
                                                      $fptr-ptr:(jobject array), \
                                                      NULL) } |]

GET_ARRAY_ELEMENTS(Boolean, Word8, jboolean)
GET_ARRAY_ELEMENTS(Byte, CChar, jbyte)
GET_ARRAY_ELEMENTS(Char, Word16, jchar)
GET_ARRAY_ELEMENTS(Short, Int16, jshort)
GET_ARRAY_ELEMENTS(Int, Int32, jint)
GET_ARRAY_ELEMENTS(Long, Int64, jlong)
GET_ARRAY_ELEMENTS(Float, Float, jfloat)
GET_ARRAY_ELEMENTS(Double, Double, jdouble)

getStringChars :: JString -> IO (Ptr Word16)
getStringChars jstr = throwIfJNull jstr $ withJNIEnv $ \env ->
    throwIfNull ArrayCopyFailed $
    [CU.exp| const jchar* {
      (*$(JNIEnv *env))->GetStringChars($(JNIEnv *env),
                                        $fptr-ptr:(jstring jstr),
                                        NULL) } |]

#define GET_ARRAY_REGION(name, hs_argtype, c_argtype) \
get/**/name/**/ArrayRegion :: J/**/name/**/Array -> Int32 -> Int32 -> Ptr hs_argtype -> IO (); \
get/**/name/**/ArrayRegion array start len buf = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Get/**/name/**/ArrayRegion($(JNIEnv *env), \
                                                    $fptr-ptr:(c_argtype/**/Array array), \
                                                    $(jsize start), \
                                                    $(jsize len), \
                                                    $(c_argtype *buf)) } |]

GET_ARRAY_REGION(Boolean, Word8, jboolean)
GET_ARRAY_REGION(Byte, CChar, jbyte)
GET_ARRAY_REGION(Char, Word16, jchar)
GET_ARRAY_REGION(Short, Int16, jshort)
GET_ARRAY_REGION(Int, Int32, jint)
GET_ARRAY_REGION(Long, Int64, jlong)
GET_ARRAY_REGION(Float, Float, jfloat)
GET_ARRAY_REGION(Double, Double, jdouble)

#define SET_ARRAY_REGION(name, hs_argtype, c_argtype) \
set/**/name/**/ArrayRegion :: J/**/name/**/Array -> Int32 -> Int32 -> Ptr hs_argtype -> IO (); \
set/**/name/**/ArrayRegion array start len buf = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Set/**/name/**/ArrayRegion($(JNIEnv *env), \
                                                    $fptr-ptr:(c_argtype/**/Array array), \
                                                    $(jsize start), \
                                                    $(jsize len), \
                                                    $(c_argtype *buf)) } |]

SET_ARRAY_REGION(Boolean, Word8, jboolean)
SET_ARRAY_REGION(Byte, CChar, jbyte)
SET_ARRAY_REGION(Char, Word16, jchar)
SET_ARRAY_REGION(Short, Int16, jshort)
SET_ARRAY_REGION(Int, Int32, jint)
SET_ARRAY_REGION(Long, Int64, jlong)
SET_ARRAY_REGION(Float, Float, jfloat)
SET_ARRAY_REGION(Double, Double, jdouble)

#define RELEASE_ARRAY_ELEMENTS(name, hs_argtype, c_argtype) \
release/**/name/**/ArrayElements :: J/**/name/**/Array -> Ptr hs_argtype -> IO (); \
release/**/name/**/ArrayElements (upcast -> array) xs = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Release/**/name/**/ArrayElements($(JNIEnv *env), \
                                                          $fptr-ptr:(jobject array), \
                                                          $(c_argtype *xs), \
                                                          JNI_ABORT) } |]

RELEASE_ARRAY_ELEMENTS(Boolean, Word8, jboolean)
RELEASE_ARRAY_ELEMENTS(Byte, CChar, jbyte)
RELEASE_ARRAY_ELEMENTS(Char, Word16, jchar)
RELEASE_ARRAY_ELEMENTS(Short, Int16, jshort)
RELEASE_ARRAY_ELEMENTS(Int, Int32, jint)
RELEASE_ARRAY_ELEMENTS(Long, Int64, jlong)
RELEASE_ARRAY_ELEMENTS(Float, Float, jfloat)
RELEASE_ARRAY_ELEMENTS(Double, Double, jdouble)

releaseStringChars :: JString -> Ptr Word16 -> IO ()
releaseStringChars jstr chars = throwIfJNull jstr $ withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->ReleaseStringChars($(JNIEnv *env),
                                            $fptr-ptr:(jstring jstr),
                                            $(jchar *chars)) } |]

getObjectArrayElement
  :: forall a o.
     (IsReferenceType a, Coercible o (J a))
  => JArray a
  -> Int32
  -> IO o
getObjectArrayElement (arrayUpcast -> array) i = throwIfJNull array $
    withJNIEnv $ \env ->
    ( (coerce :: J a -> o)
    . (unsafeCast :: JObject -> J a)
    ) <$> (objectFromPtr =<<)
    [C.exp| jobject {
      (*$(JNIEnv *env))->GetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jarray array),
                                               $(jsize i)) } |]

setObjectArrayElement
  :: forall a o.
     (IsReferenceType a, Coercible o (J a))
  => JArray a
  -> Int32
  -> o
  -> IO ()
setObjectArrayElement (arrayUpcast -> array)
                      i
                      ((coerce :: o -> J a) -> upcast -> x) =
    throwIfJNull array $
    withJNIEnv $ \env ->
    [C.exp| void {
      (*$(JNIEnv *env))->SetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jobjectArray array),
                                               $(jsize i),
                                               $fptr-ptr:(jobject x)); } |]

newDirectByteBuffer :: Ptr CChar -> Int64 -> IO JByteBuffer
newDirectByteBuffer (castPtr -> address) capacity =
    (throwIfNull NullPointerException (return address) >>) $
    withJNIEnv $ \env ->
    fmap (unsafeCast :: JObject -> JByteBuffer) $
    (objectFromPtr =<<) $
    throwIfNull DirectBufferFailed $
    [C.exp| jobject {
      (*$(JNIEnv *env))->NewDirectByteBuffer($(JNIEnv *env),
                                             $(void *address),
                                             $(jlong capacity)) } |]

getDirectBufferAddress :: JByteBuffer -> IO (Ptr CChar)
getDirectBufferAddress (upcast -> jbuffer) =
    throwIfJNull jbuffer $
    withJNIEnv $ \env ->
    fmap castPtr $
    throwIfNull DirectBufferFailed $
    [C.exp| void* {
      (*$(JNIEnv *env))->GetDirectBufferAddress($(JNIEnv *env),
                                                $fptr-ptr:(jobject jbuffer)) } |]

getDirectBufferCapacity :: JByteBuffer -> IO Int64
getDirectBufferCapacity (upcast -> jbuffer) = do
    capacity <- throwIfJNull jbuffer $
      withJNIEnv $ \env ->
      [C.exp| jlong {
        (*$(JNIEnv *env))->GetDirectBufferCapacity($(JNIEnv *env),
                                                   $fptr-ptr:(jobject jbuffer)) } |]
    if capacity >= 0 then
      return capacity
    else
      throwIO DirectBufferFailed
