module Homely
  ( module X
  , app
  , migrate
  ) where

import           RIO

import           Homely.API                (api, server)
import           Homely.Config             as X
import           Homely.DB                 as DB
import           Homely.Env                as X
import qualified Mix.Plugin.Logger         as MixLogger
import qualified Mix.Plugin.Persist.Sqlite as MixDB
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant

app :: RIO Env ()
app = do
  MixLogger.logInfo "Please accsess to localhost:8080"
  unlift <- askUnliftIO
  liftIO $ Warp.run 8080 $
    serve api $ hoistServer api (liftIO . unliftIO unlift) server

migrate :: RIO Env ()
migrate = do
  (MixDB.Config config) <- asks (view #sqlite)
  let connName = config ^. (#info . MixDB.sqlConnectionStr)
  MixLogger.logInfo (display $ "Migate SQLite for MomelyApp: " <> connName)
  MixDB.runMigrate DB.migrateAll
