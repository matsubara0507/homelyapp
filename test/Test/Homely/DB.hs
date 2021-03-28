module Test.Homely.DB where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Map                   as Map
import qualified RIO.Set                   as Set
import qualified RIO.Text                  as Text
import           RIO.Time

import           Data.Extensible
import           Homely.DB
import qualified Mix
import           Mix.Plugin.Logger         ()
import qualified Mix.Plugin.Persist.Sqlite as MixDB
import           Test.Tasty
import           Test.Tasty.Hspec

type TestEnv = Record
  '[ "logger" >: LogFunc
   , "sqlite" >: MixDB.Config
   ]

mkPlugin :: Text -> Mix.Plugin a m TestEnv
mkPlugin path = hsequence
   $ #logger <@=> pure (mkLogFunc $ \_ _ _ _ -> pure ()) -- NoLogging
  <: #sqlite <@=> MixDB.buildPluginWithoutPool path
  <: nil

withMigrateOn :: MonadUnliftIO m => Text -> m TestTree -> m TestTree
withMigrateOn path spec =
  bracket
    migrateForTest
    (\_ -> removeFile $ Text.unpack path)
    (const spec)
  where
    migrateForTest = do
      createDirectoryIfMissing True (takeDirectory $ Text.unpack path)
      Mix.run (mkPlugin path) (MixDB.runMigrate migrateAll)

tests :: IO TestTree
tests = withMigrateOn dbPath $
  testSpec "Homely.DB" $ do
    describe "insertExpense" $ do
      context "witout label" $ do
        let expect = #amount      @= 1000
                  <: #date        @= fromGregorian 2021 3 21
                  <: #description @= "test"
                  <: #labels      @= mempty
                  <: nil
        actual <- runIO $ runWithDB $ do
          idx <- insertExpense expect
          e <- findExpenseById idx
          deleteExpenseById idx
          pure e
        it "insert Expense" $ do
          fmap (view #amount) actual      `shouldBe` Just (expect ^. #amount)
          fmap (view #date) actual        `shouldBe` Just (expect ^. #date)
          fmap (view #description) actual `shouldBe` Just (expect ^. #description)
          fmap (view #labels) actual      `shouldBe` Just mempty
      context "with label" $ do
        let label1 = #name @= "hoge" <: #description @= "hogege" <: nil
            label2 = #name @= "fuga" <: #description @= "fugaga" <: nil
            expect = #amount      @= 1000
                  <: #date        @= fromGregorian 2021 3 21
                  <: #description @= "test"
                  <: #labels      @= mempty
                  <: nil
        labelIds <- runIO $ runWithDB $ Set.fromList <$> mapM insertLabel [label1, label2]
        actual <- runIO $ runWithDB $ do
          idx <- insertExpense (expect & #labels `set` labelIds)
          e <- findExpenseById idx
          deleteExpenseById idx
          mapM_ deleteLabelById $ Set.toList labelIds
          pure e
        it "insert Expense" $ do
          fmap (view #amount) actual      `shouldBe` Just (expect ^. #amount)
          fmap (view #date) actual        `shouldBe` Just (expect ^. #date)
          fmap (view #description) actual `shouldBe` Just (expect ^. #description)
          fmap (view #labels) actual      `shouldBe` Just labelIds
    describe "selectExpensesByMonth" $ do
      context "witout label" $ do
        let expect1 = #amount      @= 1000
                   <: #date        @= fromGregorian 2021 3 21
                   <: #description @= "test"
                   <: #labels      @= mempty
                   <: nil
            expect2 = #amount      @= 3000
                   <: #date        @= fromGregorian 2021 3 22
                   <: #description @= "test"
                   <: #labels      @= mempty
                   <: nil
        actual <- runIO $ runWithDB $ do
          idx1 <- insertExpense expect1
          idx2 <- insertExpense expect2
          es <- selectExpensesByMonth (2021, 3)
          deleteExpenseById idx1
          deleteExpenseById idx2
          pure es
        it "insert Expense" $
          Map.elems actual `shouldBe` [expect1, expect2]
      context "with label" $ do
        let label1 = #name @= "hoge" <: #description @= "hogege" <: nil
            label2 = #name @= "fuga" <: #description @= "fugaga" <: nil
        labelIds <- runIO $ runWithDB $ Set.fromList <$> mapM insertLabel [label1, label2]
        let expect1 = #amount      @= 1000
                   <: #date        @= fromGregorian 2021 3 21
                   <: #description @= "test"
                   <: #labels      @= labelIds
                   <: nil
            expect2 = #amount      @= 3000
                   <: #date        @= fromGregorian 2021 3 22
                   <: #description @= "test"
                   <: #labels      @= Set.take 1 labelIds
                   <: nil
        actual <- runIO $ runWithDB $ do
          idx1 <- insertExpense expect1
          idx2 <- insertExpense expect2
          es <- selectExpensesByMonth (2021, 3)
          deleteExpenseById idx1
          deleteExpenseById idx2
          mapM_ deleteLabelById $ Set.toList labelIds
          pure es
        it "insert Expense" $
          Map.elems actual `shouldBe` [expect1, expect2]
  where
    dbPath = "./tmp/test.sqlite"
    runWithDB :: RIO TestEnv a -> IO a
    runWithDB = Mix.run (mkPlugin dbPath)
