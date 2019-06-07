{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Inline.Plugin.QQMarker where

import GhcPlugins
import qualified GhcPlugins.Extras
import Language.Java.Inline.QQMarker

-- | Get the names of all markers used for java quasiquotations.
getQQMarkers :: CoreM [Name]
getQQMarkers = maybeToList <$> GhcPlugins.Extras.findTHName 'qqMarker
