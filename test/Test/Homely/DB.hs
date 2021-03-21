module Test.Homely.DB where

import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Map                   as Map
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
        actual <- runIO $ runWithDB $ do
          labels <- forM [label1, label2] $ \label -> do
            idx <- insertLabel label
            pure (idx, label)
          idx <- insertExpense (expect & #labels `set` Map.fromList labels)
          e <- findExpenseById idx
          deleteExpenseById idx
          forM_ (fst <$> labels) deleteLabelById
          pure e
        it "insert Expense" $ do
          fmap (view #amount) actual             `shouldBe` Just (expect ^. #amount)
          fmap (view #date) actual               `shouldBe` Just (expect ^. #date)
          fmap (view #description) actual        `shouldBe` Just (expect ^. #description)
          fmap (Map.elems . view #labels) actual `shouldBe` Just [label1, label2]
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
        labels <- runIO $ runWithDB $
          forM [label1, label2] $ \label -> do
            idx <- insertLabel label
            pure (idx, label)
        let expect1 = #amount      @= 1000
                   <: #date        @= fromGregorian 2021 3 21
                   <: #description @= "test"
                   <: #labels      @= Map.fromList labels
                   <: nil
            expect2 = #amount      @= 3000
                   <: #date        @= fromGregorian 2021 3 22
                   <: #description @= "test"
                   <: #labels      @= Map.fromList (take 1 labels)
                   <: nil
        actual <- runIO $ runWithDB $ do
          idx1 <- insertExpense expect1
          idx2 <- insertExpense expect2
          es <- selectExpensesByMonth (2021, 3)
          deleteExpenseById idx1
          deleteExpenseById idx2
          forM_ (fst <$> labels) deleteLabelById
          pure es
        it "insert Expense" $
          Map.elems actual `shouldBe` [expect1, expect2]
  where
    dbPath = "./tmp/test.sqlite"
    runWithDB :: RIO TestEnv a -> IO a
    runWithDB = Mix.run (mkPlugin dbPath)
