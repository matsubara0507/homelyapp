module Main where

import           RIO

import qualified Test.Homely.DB
import           Test.Tasty

main :: IO ()
main = defaultMain . testGroup "homelyapp package" =<< sequence
  [ Test.Homely.DB.tests
  ]
