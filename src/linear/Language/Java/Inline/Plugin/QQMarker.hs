{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Inline.Plugin.QQMarker where

import Data.Maybe
import GhcPlugins
import qualified GhcPlugins.Extras
import Language.Java.Inline.QQMarker
import qualified Language.Java.Inline.QQMarker.Linear as Linear

-- | Get the names of all markers used for java quasiquotations.
getQQMarkers :: CoreM [Name]
getQQMarkers = do
   ma <- GhcPlugins.Extras.findTHName 'qqMarker
   mb <- GhcPlugins.Extras.findTHName 'Linear.qqMarker
   return $ catMaybes [ma, mb]
