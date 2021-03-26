module Homely.API where

import           RIO

import           Data.Extensible
import qualified Homely.DB              as DB
import           Homely.Data.Expense    (Expense, Label)
import           Homely.Env             (Env)
import           Mix.Plugin.Logger      ()
import qualified Mix.Plugin.Logger.JSON as Mix
import           Servant

type API
      = "api" :> CRUD

type CRUD
      = "expenses" :> QueryParam' '[Required] "year" Integer :> QueryParam' '[Required] "month" Int :> Get '[JSON] (Map Int64 Expense)
   :<|> "expenses" :> ReqBody '[JSON, JSON] Expense :> Post '[JSON] Int64
   :<|> "expenses" :> QueryParam' '[Required] "expenseId" Int64 :> Delete '[JSON] NoContent
   :<|> "labels"   :> Get '[JSON] (Map Int64 Label)
   :<|> "labels"   :> ReqBody '[JSON, JSON] Label :> Post '[JSON] Int64
   :<|> "labels"   :> QueryParam' '[Required] "labelId" Int64 :> Delete '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: ServerT API (RIO Env)
server = getExpenses
    :<|> createExpense
    :<|> deleteExpense
    :<|> getLabels
    :<|> createLabel
    :<|> deleteLabel
  where
    getExpenses y m = do
      Mix.logDebugR "GET: api/expenses" (#year @= y <: #month @= m <: nil)
      DB.selectExpensesByMonth (y, m)

    createExpense expense = do
      Mix.logDebugR "POST: api/expenses" expense
      DB.insertExpense expense

    deleteExpense expenseId = do
      Mix.logDebugR "DELETE: api/expenses" (#id @= expenseId <: nil)
      DB.deleteExpenseById expenseId
      pure NoContent

    getLabels = do
      Mix.logDebugR "GET: api/labels" nil
      DB.selectLabelAll

    createLabel label = do
      Mix.logDebugR "POST: api/labels" label
      DB.insertLabel label

    deleteLabel labelId = do
      Mix.logDebugR "DELETE: api/labels" (#id @= labelId <: nil)
      DB.deleteLabelById labelId
      pure NoContent


