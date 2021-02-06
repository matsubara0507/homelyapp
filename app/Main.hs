module Main where

import           Paths_homelyapp         (version)
import           RIO

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import qualified Data.Version           as Version
import           GetOpt                 (withGetOpt')
import           Homely.Cmd
import           Homely.Env
import           Mix
import           Mix.Plugin.Logger      as MixLogger

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (fromString $ Version.showVersion version <> "\n")
     | otherwise     -> runCmd r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts _path = Mix.run plugin cmd
  where
    plugin :: Mix.Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin logOpts
       <: nil
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
