{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Homely.API where

import           RIO
import           RIO.FilePath                (takeDirectory)

import           Data.Extensible
import           Data.FileEmbed              (embedDir)
import qualified Homely.DB                   as DB
import           Homely.Data.Expense         (Expense, Label)
import           Homely.Env                  (Env)
import           Mix.Plugin.Logger           ()
import qualified Mix.Plugin.Logger.JSON      as Mix
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H hiding (title)

type API
      = Get '[HTML] H.Html
   :<|> "static" :> Raw
   :<|> "api" :> CRUD

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
server = indexHtml
    :<|> serveDirectoryEmbedded $(embedDir (takeDirectory MAINJS_FILE))
    :<|> getExpenses
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

indexHtml :: RIO Env H.Html
indexHtml = do
  Mix.logDebugR "GET: index" nil
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet bulma
      stylesheet fontawesome
      H.meta ! H.name "viewpoint" ! H.content "width=device-width, initial-scale=1"
      H.title $ H.text "Homely App"
    H.div ! H.class_ "Box text-center mt-3 container-sm" $ do
      H.div ! H.class_ "Box-header" $
        H.h1 ! H.class_ "Box-title" $ H.text "Homely App"
      H.div ! H.class_ "Box-Body" ! H.id "main" $ H.text ""
    H.script ! H.type_ "text/javascript" ! H.src "/static/main.js" $ H.text ""
    H.script ! H.type_ "text/javascript" $ H.text "Elm.Main.init({node: document.getElementById('main'), flags: {}});"
  where
    bulma = "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css"
    fontawesome = "https://use.fontawesome.com/releases/v5.15.3/css/all.css"

    stylesheet :: H.AttributeValue -> H.Html
    stylesheet url =
      H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"
