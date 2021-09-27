module Homely.Env where

import           RIO

import           Data.Extensible
import           Mix.Plugin.Logger         ()
import qualified Mix.Plugin.Persist.Sqlite as MixDB

type Env = Record
  '[ "logger" >: LogFunc
   , "sqlite" >: MixDB.Config
   ]
