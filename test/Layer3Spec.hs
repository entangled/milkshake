-- ~\~ language=Haskell filename=test/Layer3Spec.hs
-- ~\~ begin <<docs/milkshake.md|test/Layer3Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer3Spec (spec) where

import RIO
import Test.Hspec

import Milkshake.Data (readStmts, Config(..), stmtsToConfig)
import Milkshake.Run (enter, loadIncludes, immediateActions)

import Development.Shake (shake, shakeOptions, want)
import Util (runInTmp)

spec :: Spec
spec = describe "Layer3" $ do
    it "can load a configuration" $ runInTmp ["./test/Layer3/*"] $ do
        cfg <- stmtsToConfig <$> readStmts "./test1.dhall"
        mainTarget cfg `shouldSatisfy` (not . null)
        gen <- stmtsToConfig <$> readStmts "./template.dhall 42"
        actions gen `shouldSatisfy` (not . null)
    it "can run all actions" $ runInTmp ["./test/Layer3/*"] $ do
        cfg <- loadIncludes . stmtsToConfig =<< readStmts "./test1.dhall"
        actions <- either throwM return $ immediateActions cfg
        shake shakeOptions (mapM_ enter actions >> want (mainTarget cfg))
        result <- readFileUtf8 "answer.txt"
        result `shouldBe` "42\n"
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|test/Layer3Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer3Spec (spec) where

import RIO
import Test.Hspec

import Milkshake.Data (readStmts, Config(..), stmtsToConfig)
import Milkshake.Run (enter, loadIncludes, immediateActions)

import Development.Shake (shake, shakeOptions, want)
import Util (runInTmp)

spec :: Spec
spec = describe "Layer3" $ do
    it "can load a configuration" $ runInTmp ["./test/Layer3/*"] $ do
        cfg <- stmtsToConfig <$> readStmts "./test1.dhall"
        mainTarget cfg `shouldSatisfy` (not . null)
        gen <- stmtsToConfig <$> readStmts "./template.dhall 42"
        actions gen `shouldSatisfy` (not . null)
    it "can run all actions" $ runInTmp ["./test/Layer3/*"] $ do
        cfg <- loadIncludes . stmtsToConfig =<< readStmts "./test1.dhall"
        actions <- either throwM return $ immediateActions cfg
        shake shakeOptions (mapM_ enter actions >> want (mainTarget cfg))
        result <- readFileUtf8 "answer.txt"
        result `shouldBe` "42\n"
-- ~\~ end
