module Homely
  ( module X
  , app
  , migrate
  ) where

import           RIO

import           Homely.Config             as X
import           Homely.DB                 as DB
import           Homely.Env                as X
import qualified Mix.Plugin.Logger         as MixLogger
import qualified Mix.Plugin.Persist.Sqlite as MixDB


app :: RIO Env ()
app = showNotImpl

migrate :: RIO Env ()
migrate = do
  (MixDB.Config config) <- asks (view #sqlite)
  let connName = config ^. #info ^. MixDB.sqlConnectionStr
  MixLogger.logInfo (display $ "Migate SQLite for MomelyApp: " <> connName)
  MixDB.runMigrate DB.migrateAll

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
