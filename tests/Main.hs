module Main where

import Language.Java (withJVM)
import qualified Spec
import qualified LinearSpec
import Test.Hspec

main :: IO ()
main = withJVM [] $ hspec $ do
    Spec.spec
    LinearSpec.spec
