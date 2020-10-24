-- ~\~ language=Haskell filename=test/Layer3Spec.hs
-- ~\~ begin <<lit/index.md|test/Layer3Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer3Spec (spec) where

import RIO
import Test.Hspec
import qualified RIO.Map as M

import Milkshake.Data
    ( Trigger(..), Action(..), Target(..)
    , readStmts, Config(..), stmtsToConfig)
import Milkshake.Run (enter)

import Development.Shake (shake, shakeOptions, ShakeOptions(..), Verbosity(..))
import Util (runInTmp)

fromTrigger :: Config -> Trigger -> Either Text Action
fromTrigger cfg Trigger{..} = case rule of
    Just r  -> Right $ Action target dependency (r target dependency)
    Nothing -> Left $ "No such rule: " <> name
    where rule = (rules cfg) M.!? name

data MilkShakeError
    = ConfigError Text
    deriving (Show, Eq)

instance Exception MilkShakeError

spec :: Spec
spec = describe "Layer3" $ do
    it "can load a configuration" $ runInTmp ["./test/Layer3/*"] $ do
        cfg <- stmtsToConfig <$> readStmts "./test1.dhall"
        (actions cfg) `shouldSatisfy` any (\Action{..} -> target == [Phony "main"])
    -- it "can run all actions" $ runInTmp ["./test/Layer1/hello.c", "./test/Layer2/*"] $ do
    --     cfg <- stmtsToConfig <$> readStmts "./test1.dhall"
    --     case mapM (fromTrigger cfg) (triggers cfg) of
    --         Left e -> throwM (ConfigError e)
    --         Right as -> do
    --             let actionList = (actions cfg) <> as
    --             shake (shakeOptions { shakeVerbosity = Diagnostic }) (mapM_ enter actionList)
    --             result <- readFileUtf8 "out.txt"
    --             result `shouldBe` "Hello, World!\n"
-- ~\~ end
