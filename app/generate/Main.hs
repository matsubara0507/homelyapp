{-# LANGUAGE TypeApplications #-}

module Main where

import           RIO
import qualified RIO.Text            as Text

import           Elm.Mapping
import           Homely.API          (CRUD)
import           Homely.Data.Expense (Expense, Label)
import           Servant             ((:>))
import           Servant.Elm.Mapping hiding (defElmImports)
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

defElmImports :: Text
defElmImports =
  Text.unlines
    [ "import Json.Decode"
    , "import Json.Encode exposing (Value)"
    , "-- The following module comes from bartavelle/json-helpers"
    , "import Json.Helpers exposing (..)"
    , "import Dict exposing (Dict)"
    , "import Set exposing (Set)"
    , "import Http"
    , "import String"
    , "import Url.Builder"
    ]
