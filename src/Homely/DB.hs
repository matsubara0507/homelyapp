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
  , selectLabelAll
  , deleteExpenseById
  , deleteLabelById
  ) where


import           RIO
import qualified RIO.Map                         as Map
import qualified RIO.Set                         as Set
import           RIO.Time

import           Data.Extensible                 hiding (fromRecord)
import           Database.Esqueleto.Experimental hiding (set, (^.))
import qualified Database.Esqueleto.Experimental as DB
import           Database.Persist.TH
import           Homely.Data.Expense             (Expense, ExpenseId, Label,
                                                  LabelId)
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

insertExpense :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Expense -> m ExpenseId
insertExpense expense = MixDB.run $ do
  expenseId <- insert expenseData
  insertMany_ $ ExpenseLabelRel expenseId . toSqlKey <$> Set.toList (expense ^. #labels)
  pure $ fromSqlKey expenseId
  where
    expenseData = ExpenseData
      (expense ^. #amount)
      (UTCTime (expense ^. #date) 0)
      (expense ^. #description)
      zeroTime
      zeroTime
    zeroTime = UTCTime (ModifiedJulianDay 0) 0

insertLabel :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => Label -> m LabelId
insertLabel label = MixDB.run $ fromSqlKey <$> insert (LabelData (label ^. #name) (label ^. #description))

findExpenseById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => ExpenseId -> m (Maybe Expense)
findExpenseById idx = MixDB.run $ do
  es <- select $ do
    e <- from $ Table @ExpenseData
    where_ (e DB.^. ExpenseDataId ==. val (toSqlKey idx))
    limit 1
    pure e
  case listToMaybe es of
    Nothing -> pure Nothing
    Just e  -> do
      lids <- select $ do
        el <- from $ Table @ExpenseLabelRel
        where_ ((el DB.^. ExpenseLabelRelExpenseId) ==. val (entityKey e))
        pure (el DB.^. ExpenseLabelRelLabelId)
      pure $ Just (snd $ fromExpenseDataWith (Map.singleton idx $ Set.fromList $ fromSqlKey . unValue <$> lids) e)

findLabelById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => LabelId -> m (Maybe Label)
findLabelById idx = MixDB.run $ do
  ls <- select $ do
    l <- from $ Table @LabelData
    where_ (l DB.^. LabelDataId ==. val (toSqlKey idx))
    limit 1
    pure l
  pure $ toLabel . entityVal <$> listToMaybe ls

selectExpensesByMonth
  :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)
  => (Integer, Int) -> m (Map ExpenseId Expense)
selectExpensesByMonth (y, m) =
  MixDB.run $ do
    es <- select $ do
      e <- from $ Table @ExpenseData
      where_ (between (e DB.^. ExpenseDataDate) (val startDate, val endDate))
      pure e
    let eIds = fmap entityKey es
    els <- select $ do
      el <- from $ Table @ExpenseLabelRel
      where_ ((el DB.^. ExpenseLabelRelExpenseId) `in_` valList eIds) -- ToDo: eIds size is max 1000
      pure el
    pure $ Map.fromList (fromExpenseDataWith (toLabelIdsMap $ fmap entityVal els) <$> es)
  where
    startDay  = fromGregorian y m 1
    startDate = UTCTime startDay 0
    endDate   = addUTCTime (-1) $ UTCTime (addGregorianMonthsClip 1 startDay) 0

selectLabelAll :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => m (Map LabelId Label)
selectLabelAll = MixDB.run $ do
  ls <- select $ from $ Table @LabelData
  pure (Map.fromList $ fmap (\l -> (fromSqlKey $ entityKey l, toLabel $ entityVal l)) ls)

fromExpenseDataWith :: Map ExpenseId (Set LabelId) -> Entity ExpenseData -> (ExpenseId, Expense)
fromExpenseDataWith labelMap e =
  ( fromSqlKey $ entityKey e
  , toEpense (entityVal e) $ fromMaybe mempty (Map.lookup (fromSqlKey $ entityKey e) labelMap)
  )

toEpense :: ExpenseData -> Set LabelId -> Expense
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

toLabelIdsMap :: [ExpenseLabelRel] -> Map ExpenseId (Set LabelId)
toLabelIdsMap = Map.fromListWith (<>) . fmap (\(ExpenseLabelRel eid lid) -> (fromSqlKey eid, Set.singleton $ fromSqlKey lid))

deleteExpenseById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => ExpenseId  -> m ()
deleteExpenseById idx = MixDB.run $ do
  e <- select $ do
    e <- from $ Table @ExpenseData
    where_ (e DB.^. ExpenseDataId ==. val (toSqlKey idx))
    limit 1
    pure e
  forM_ e $ deleteCascade . entityKey

deleteLabelById :: (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m) => LabelId -> m ()
deleteLabelById idx = MixDB.run $
  delete $ do
    l <- from $ Table @LabelData
    where_ (l DB.^. LabelDataId ==. val (toSqlKey idx))
    pure ()
