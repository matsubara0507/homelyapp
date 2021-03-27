module Homely.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml       as Y

type Config = Record
  '[ "sqlite_path" >: FilePath
   , "static_path" >: FilePath
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow
