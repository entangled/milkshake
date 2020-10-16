-- ~\~ language=Haskell filename=test/Layer1Spec.hs
-- ~\~ begin <<lit/index.md|test/Layer1Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer1Spec (spec) where

import RIO
import Test.Hspec

import Milkshake.Data (Action(..), Target(..))
import Milkshake.Run (enter)
import Dhall (auto, input)

import Development.Shake (shake, shakeOptions, ShakeOptions(..), Verbosity(..))
import Util (runInTmp)

spec :: Spec
spec = describe "Layer1" $ do
    it "can load a list of actions" $ runInTmp ["./test/Layer1/*"] $ do
        actionList <- input auto "./test1.dhall" :: IO [Action]
        actionList `shouldSatisfy` any (\a -> target (a :: Action) == [ Phony "main" ])
    it "can run a list of actions" $ runInTmp ["./test/Layer1/*"] $ do
        actionList <- input auto "./test1.dhall" :: IO [Action]
        shake (shakeOptions {shakeVerbosity = Diagnostic})
              (mapM_ enter actionList)
        result <- readFileUtf8 "out.txt"
        result `shouldBe` "Hello, World!\n"
-- ~\~ end
