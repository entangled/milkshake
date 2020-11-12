-- ~\~ language=Haskell filename=test/Milkshake/MonitorSpec.hs
-- ~\~ begin <<lit/index.md|test/Milkshake/MonitorSpec.hs>>[0]
module Milkshake.MonitorSpec (spec) where

import RIO
import RIO.Directory (canonicalizePath)
import RIO.File (writeBinaryFile)

import Test.Hspec
import Util (runInTmp, runWithLogger)

import Milkshake.Monitor

data MyEvent = Ping deriving (Show, Eq)

ping :: MonadIO m => Event -> m MyEvent
ping = const $ return Ping

spec :: Spec
spec = describe "Monitor" $ do
    it "monitors file creation" $ runInTmp [] $ do
        signal <- runWithLogger $ withWatchManager (\wm -> do
                chan <- newChan
                stop <- monitor wm chan [(["./*"], return)]
                writeBinaryFile "test.txt" mempty
                signal <- timeout 1000 $ readChan chan
                stop
                return signal)
        abs_filename <- canonicalizePath "./test.txt"
        signal `shouldSatisfy` \case
            Just (Added path _ _) -> path == abs_filename
            _ -> False
-- ~\~ end
