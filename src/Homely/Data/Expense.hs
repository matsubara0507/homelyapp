{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Homely.Data.Expense where

import           RIO
import           RIO.Time

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Orphans                     ()

type Expense = Record
  '[ "amount"      >: Int -- 円
   , "date"        >: Day
   , "description" >: Text
   , "labels"      >: Map Int64 Label
   ]

instance IsElmType Expense where
  compileElmType = compileElmRecordTypeWith "Expense"

instance IsElmDefinition Expense where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Expense"

type Label = Record
  '[ "name"        >: Text
   , "description" >: Text
   ]

instance IsElmType Label where
  compileElmType = compileElmRecordTypeWith "Label"

instance IsElmDefinition Label where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Label"
