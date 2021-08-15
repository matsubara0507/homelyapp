module Main where

import           Paths_homelyapp           (version)
import           RIO
import           RIO.FilePath              (isRelative, takeDirectory, (</>))

import           Configuration.Dotenv      (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import qualified Data.Version              as Version
import           GetOpt                    (withGetOpt')
import qualified Homely
import           Mix
import           Mix.Plugin.Logger         as MixLogger
import qualified Mix.Plugin.Persist.Sqlite as MixDB

main :: IO ()
main = withGetOpt' "[options] [config filepath]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (fromString $ Version.showVersion version <> "\n")
     | otherwise     -> runCmd r (fromMaybe "./.homely.yaml" $ listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #migrate @= migrateOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "migrate" >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

migrateOpt :: OptDescr' Bool
migrateOpt = optFlag [] ["migrate"] "Migrate SQLite"

runCmd :: Options -> FilePath -> IO ()
runCmd opts path = do
  config <- Homely.readConfig path
  let sqlitePath =
        if isRelative (config ^. #sqlite_path) then
          takeDirectory path </> config ^. #sqlite_path
        else
          config ^. #sqlite_path
      plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin logOpts
       <: #sqlite <@=> MixDB.buildPlugin (fromString sqlitePath) 2
       <: nil
  if opts ^. #migrate then
    Mix.run plugin Homely.migrate
  else
    Mix.run plugin Homely.app
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
