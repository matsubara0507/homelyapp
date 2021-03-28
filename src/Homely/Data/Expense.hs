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

type ExpenseId = Int64

type Expense = Record
  '[ "amount"      >: Int -- 円
   , "date"        >: Day
   , "description" >: Text
   , "labels"      >: Set LabelId
   ]

instance IsElmType Expense where
  compileElmType = compileElmRecordTypeWith "Expense"

instance IsElmDefinition Expense where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Expense"

type LabelId = Int64

type Label = Record
  '[ "name"        >: Text
   , "description" >: Text
   ]

instance IsElmType Label where
  compileElmType = compileElmRecordTypeWith "Label"

instance IsElmDefinition Label where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Label"
