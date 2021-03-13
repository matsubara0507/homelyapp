{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Homely.Data.Expense where


import           RIO
import           RIO.Time

import           Data.Extensible

type Expense = Record
  '[ "amount"      >: Int -- 円
   , "date"        >: Day
   , "description" >: Text
   , "labels"      >: Map Int64 Label
   ]

type Label = Record
  '[ "name"        >: Text
   , "description" >: Text
   ]
