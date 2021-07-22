-- ~\~ language=Haskell filename=test/Layer2Spec.hs
-- ~\~ begin <<lit/milkshake.md|test/Layer2Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer2Spec (spec) where

import RIO
-- import qualified RIO.Text as T
import Test.Hspec

import Milkshake.Data (Action(..), Target(..), readStmts, Config(..), stmtsToConfig)
import Milkshake.Run (enter, fromTrigger)

import Development.Shake (shake, shakeOptions)
import Util (runInTmp)

spec :: Spec
spec = describe "Layer2" $ do
    it "can load a configuration" $ runInTmp ["./test/Layer2/*"] $ do
        cfg <- stmtsToConfig <$> readStmts "./test2.dhall"
        (actions cfg) `shouldSatisfy` any (\Action{..} -> target == [Phony "main"])
    it "can run all actions" $ runInTmp ["./test/Layer1/hello.c", "./test/Layer2/*"] $ do
        cfg <- stmtsToConfig <$> readStmts "./test2.dhall"
        (actions cfg) `shouldSatisfy` any (\Action{..} -> target == [Phony "main"])
        case mapM (fromTrigger cfg) (triggers cfg) of
            Left e -> throwM e
            Right as -> do
                let actionList = (actions cfg) <> as
                shake shakeOptions (mapM_ enter actionList)
                result <- readFileUtf8 "out.txt"
                result `shouldBe` "Hello, World!\n"
-- ~\~ end
