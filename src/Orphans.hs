{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           RIO
import           RIO.Time

import           Elm.Mapping
import           Servant     (NoContent)

-- ToDo
instance IsElmType Int64 where
  compileElmType _ = toElmType (Proxy @ Int)

instance IsElmType Day where
  compileElmType _ = toElmType (Proxy @ String)

instance IsElmType a => IsElmType (Set a) where
  compileElmType _ = ETyApp (ETyCon $ ETCon "Set") (compileElmType (Proxy @ a))

instance IsElmType NoContent where
  compileElmType _ = toElmType (Proxy @ ())

