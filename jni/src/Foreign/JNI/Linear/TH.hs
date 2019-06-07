{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.JNI.Linear.TH where

import Data.Coerce
import Foreign.JNI
import Foreign.JNI.Types
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

-- |
--
-- > #define GET_FIELD(name, hs_rettype) \
-- > get/**/name/**/Field :: Coercible o (J a) => o ->. JFieldID -> Linear.IO (o, Unrestricted hs_rettype); \
-- > get/**/name/**/Field = Unsafe.toLinear $ \obj field -> \
-- >     Linear.fromSystemIO $ JNI.get/**/name/**/Field obj field <&> \
-- >       \v -> (obj, Unrestricted v)
--
generateGetField :: String -> Name -> DecsQ
generateGetField name hs_rettype = do
  let fname = "get" ++ name ++ "Field"
      functionName = mkName fname
      jniFunctionName = mkName $ "JNI." ++ fname
  let t = [t| forall o a.
                  Coercible o (J a)
               => o
               ->. JFieldID
               -> Linear.IO (o, Unrestricted $(conT hs_rettype))
            |]
  sig <- sigD functionName t
  let e = [| Unsafe.toLinear $ \obj field ->
               Linear.fromSystemIO $
                 $(varE jniFunctionName) obj field <&>
                   \v -> (obj, Unrestricted v)
           |]
  fun <- funD functionName [clause [] (normalB e) []]
  return [sig, fun]
