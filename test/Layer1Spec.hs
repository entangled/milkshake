-- ~\~ language=Haskell filename=test/Layer1Spec.hs
-- ~\~ begin <<lit/milkshake.md|test/Layer1Spec.hs>>[init]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer1Spec (spec) where

import RIO
import Test.Hspec

import Milkshake.Data (Action(..), Target(..))
import Milkshake.Run (enter)
import Dhall (auto, input)

import Development.Shake (shake, shakeOptions)
import Util (runInTmp)

spec :: Spec
spec = do
    describe "Layer1" $ do
        it "can load a list of actions" $ runInTmp ["./test/Layer1/*"] $ do
            actionList <- input auto "./test1.dhall" :: IO [Action]
            actionList `shouldSatisfy` any (\a -> target (a :: Action) == [ Phony "main" ])
        it "can run a list of actions" $ runInTmp ["./test/Layer1/*"] $ do
            actionList <- input auto "./test1.dhall" :: IO [Action]
            shake shakeOptions (mapM_ enter actionList)
            result <- readFileUtf8 "out.txt"
            result `shouldBe` "Hello, World!\n"
    describe "Virtual Targets" $ do
        it "can load" $ runInTmp ["./test/Layer1/*"] $ do
            actionList <- input auto "./test2.dhall" :: IO [Action]
            actionList `shouldSatisfy` any (\a -> target (a :: Action) == [ Phony "main" ])
-- ~\~ end
