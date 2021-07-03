{-# LANGUAGE ConstraintKinds           #-}
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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
  expenseId ExpenseDataId OnDeleteCascade
  labelId LabelDataId OnDeleteCascade
  deriving Show
|]

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

type SQLitable m env = (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)

selectLabelAll :: SQLitable m env => m (Map LabelId Label)
selectLabelAll = MixDB.run $ do
  labels <- select $ from $ table @LabelData
  pure $ Map.fromList (liftA2 (,) (fromSqlKey . entityKey) (toLabel . entityVal) <$> labels)

findLabelById :: SQLitable m env => LabelId -> m (Maybe Label)
findLabelById idx =
  MixDB.run $ fmap toLabel <$> get (toSqlKey idx)

insertLabel :: SQLitable m env => Label -> m LabelId
insertLabel label =
  MixDB.run $ fromSqlKey <$> insert (LabelData (label ^. #name) (label ^. #description))

deleteLabelById :: SQLitable m env => LabelId -> m ()
deleteLabelById idx =
  MixDB.run $ deleteKey (toSqlKey idx :: Key LabelData)

findExpenseById :: SQLitable m env => ExpenseId -> m (Maybe Expense)
findExpenseById idx = MixDB.run $ do
  expense <- get $ toSqlKey idx
  for expense $ \e -> do
    lids <- select $ do
      el <- from $ table @ExpenseLabelRel
      where_ ((el DB.^. ExpenseLabelRelExpenseId) ==. val (toSqlKey idx))
      pure (el DB.^. ExpenseLabelRelLabelId)
    pure $ toEpense e (Set.fromList $ fromSqlKey . unValue <$> lids)

insertExpense :: SQLitable m env => Expense -> m ExpenseId
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

deleteExpenseById :: SQLitable m env => ExpenseId  -> m ()
deleteExpenseById idx =
  MixDB.run $ deleteKey (toSqlKey idx :: Key ExpenseData)

selectExpensesByMonth :: SQLitable m env => (Integer, Int) -> m (Map ExpenseId Expense)
selectExpensesByMonth (y, m) =
  MixDB.run $ do
    es <- select $ do
      e <- from $ table @ExpenseData
      where_ (between (e DB.^. ExpenseDataDate) (val startDate, val endDate))
      pure e
    let eIds = fmap entityKey es
    els <- select $ do
      el <- from $ table @ExpenseLabelRel
      where_ ((el DB.^. ExpenseLabelRelExpenseId) `in_` valList eIds) -- ToDo: eIds size is max 1000
      pure el
    pure $ Map.fromList (fromExpenseDataWith (toLabelIdsMap $ fmap entityVal els) <$> es)
  where
    startDay  = fromGregorian y m 1
    startDate = UTCTime startDay 0
    endDate   = addUTCTime (-1) $ UTCTime (addGregorianMonthsClip 1 startDay) 0

fromExpenseDataWith :: Map ExpenseId (Set LabelId) -> Entity ExpenseData -> (ExpenseId, Expense)
fromExpenseDataWith labelMap e =
  ( fromSqlKey $ entityKey e
  , toEpense (entityVal e) $ fromMaybe mempty (Map.lookup (fromSqlKey $ entityKey e) labelMap)
  )

toLabelIdsMap :: [ExpenseLabelRel] -> Map ExpenseId (Set LabelId)
toLabelIdsMap = Map.fromListWith (<>) . fmap (\(ExpenseLabelRel eid lid) -> (fromSqlKey eid, Set.singleton $ fromSqlKey lid))
