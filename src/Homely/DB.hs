{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Homely.DB
  ( migrateAll
  , insertExpense
  , insertLabel
  , findExpenseById
  , findLabelById
  , selectExpensesByMonth
  , deleteExpenseById
  , deleteLabelById
  ) where


import           RIO
import qualified RIO.Map                         as Map
import           RIO.Time

import           Data.Extensible                 hiding (fromRecord)
import           Database.Esqueleto.Experimental hiding (set, (^.))
import qualified Database.Esqueleto.Experimental as DB
import           Database.Persist.TH
import           Homely.Data.Expense             (Expense, Label)
import qualified Mix.Plugin.Persist.Sqlite       as MixDB

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ExpenseData
  amount Int
  date UTCTime
  description Text
  created UTCTime default=CURRENT_TIME
  updated UTCTime default=CURRENT_TIME
  deriving Show

LabelData
  name Text
  description Text
  deriving Show

ExpenseLabelRel
  expenseId ExpenseDataId
  labelId LabelDataId
  deriving Show
|]

insertExpense :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Expense -> m Int64
insertExpense expense = MixDB.run $ do
  expenseId <- insert expenseData
  insertMany_ $ ExpenseLabelRel expenseId . toSqlKey <$> Map.keys (expense ^. #labels)
  pure $ fromSqlKey expenseId
  where
    expenseData = ExpenseData
      (expense ^. #amount)
      (UTCTime (expense ^. #date) 0)
      (expense ^. #description)
      zeroTime
      zeroTime
    zeroTime = UTCTime (ModifiedJulianDay 0) 0

insertLabel :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Label -> m Int64
insertLabel label = MixDB.run $ fromSqlKey <$> insert (LabelData (label ^. #name) (label ^. #description))

findExpenseById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Int64 -> m (Maybe Expense)
findExpenseById idx = MixDB.run $ do
  es <- select $ do
    e <- from $ Table @ExpenseData
    where_ (e DB.^. ExpenseDataId ==. val (toSqlKey idx))
    limit 1
    pure e
  case listToMaybe es of
    Nothing -> pure Nothing
    Just e  -> do
      ls <- select $ do
        (el :& l) <-
          from $ Table @ExpenseLabelRel
            `InnerJoin` Table @LabelData `DB.on` (\(el :& l) -> el DB.^. ExpenseLabelRelLabelId ==. l DB.^. LabelDataId)
        where_ ((el DB.^. ExpenseLabelRelExpenseId) ==. val (entityKey e))
        pure (el DB.^. ExpenseLabelRelExpenseId, l)
      pure $ Just (snd $ fromExpenseDataWith (toLabelMap ls) e)

findLabelById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Int64 -> m (Maybe Label)
findLabelById idx = MixDB.run $ do
  ls <- select $ do
    l <- from $ Table @LabelData
    where_ (l DB.^. LabelDataId ==. val (toSqlKey idx))
    limit 1
    pure l
  pure $ toLabel . entityVal <$> listToMaybe ls

selectExpensesByMonth
  :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => (Integer, Int) -> m (Map Int64 Expense)
selectExpensesByMonth (y, m) =
  MixDB.run $ do
    es <- select $ do
      e <- from $ Table @ExpenseData
      where_ (between (e DB.^. ExpenseDataDate) (val startDate, val endDate))
      pure e
    let eIds = fmap entityKey es
    ls <- select $ do
      (el :& l) <-
        from $ Table @ExpenseLabelRel
          `InnerJoin` Table @LabelData `DB.on` (\(el :& l) -> el DB.^. ExpenseLabelRelLabelId ==. l DB.^. LabelDataId)
      where_ ((el DB.^. ExpenseLabelRelExpenseId) `in_` valList eIds) -- ToDo: eIds size is max 1000
      pure (el DB.^. ExpenseLabelRelExpenseId, l)
    pure $ Map.fromList (fromExpenseDataWith (toLabelMap ls) <$> es)
  where
    startDay  = fromGregorian y m 1
    startDate = UTCTime startDay 0
    endDate   = addUTCTime (-1) $ UTCTime (addGregorianMonthsClip 1 startDay) 0

fromExpenseDataWith :: Map Int64 (Map Int64 Label) -> Entity ExpenseData -> (Int64, Expense)
fromExpenseDataWith labelMap e =
  ( fromSqlKey $ entityKey e
  , toEpense (entityVal e) $ fromMaybe mempty (Map.lookup (fromSqlKey $ entityKey e) labelMap)
  )

toEpense :: ExpenseData -> Map Int64 Label -> Expense
toEpense (ExpenseData amount date description _ _) ls
    = #amount      @= amount
    <: #date        @= utctDay date
    <: #description @= description
    <: #labels      @= ls
    <: nil

toLabel :: LabelData -> Label
toLabel (LabelData name description)
    = #name        @= name
    <: #description @= description
    <: nil

toLabelMap :: [(Value (Key ExpenseData), Entity LabelData)] -> Map Int64 (Map Int64 Label)
toLabelMap = Map.fromListWith Map.union . fmap (\(k, l) -> (fromSqlKey $ unValue k, toSingleton l))
  where
    toSingleton l = Map.singleton (fromSqlKey $ entityKey l) (toLabel $ entityVal l)

deleteExpenseById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Int64 -> m ()
deleteExpenseById idx = MixDB.run $ do
  e <- select $ do
    e <- from $ Table @ExpenseData
    where_ (e DB.^. ExpenseDataId ==. val (toSqlKey idx))
    limit 1
    pure e
  forM_ e $ deleteCascade . entityKey

deleteLabelById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Int64 -> m ()
deleteLabelById idx = MixDB.run $
  delete $ do
    l <- from $ Table @LabelData
    where_ (l DB.^. LabelDataId ==. val (toSqlKey idx))
    pure ()
