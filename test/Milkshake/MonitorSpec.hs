-- ~\~ language=Haskell filename=test/Milkshake/MonitorSpec.hs
-- ~\~ begin <<lit/index.md|test/Milkshake/MonitorSpec.hs>>[0]
module Milkshake.MonitorSpec (spec) where

import RIO
import RIO.File (writeBinaryFile)

import Test.Hspec
import Util (runInTmp)

import Milkshake.Monitor

data MyEvent = Ping deriving (Show, Eq)

ping :: MonadIO m => Event -> m MyEvent
ping = const $ return Ping

spec :: Spec
spec = describe "Monitor" $ do
    it "monitors file creation" $ runInTmp [] $ withWatchManager (\wm -> do
        chan <- newChan
        writeBinaryFile "test.txt" mempty
        stop <- monitor wm chan [(["./*"], ping)]
        writeBinaryFile "test.txt" mempty
        signal <- readChan chan
        signal `shouldBe` Ping
        stop)
-- ~\~ end
