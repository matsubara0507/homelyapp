{-# LANGUAGE TypeApplications #-}

module Main where

import           RIO

import           Elm.Mapping
import           Homely.API          (CRUD)
import           Homely.Data.Expense (Expense, Label)
import           Servant             ((:>))
import           Servant.Elm.Mapping
import           System.Environment  (getArgs)

main :: IO ()
main = do
  dirPath <- fromMaybe "elm-src" . listToMaybe <$> getArgs
  generateElmModuleWith
    defElmOptions
    ["Generated", "API"]
    defElmImports
    dirPath
    [ DefineElm (Proxy @ Expense)
    , DefineElm (Proxy @ Label)
    ]
    (Proxy @ ("api" :> CRUD))
