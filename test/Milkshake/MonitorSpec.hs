-- ~\~ language=Haskell filename=test/Milkshake/MonitorSpec.hs
-- ~\~ begin <<lit/index.md|test/Milkshake/MonitorSpec.hs>>[0]
{-# LANGUAGE MultiParamTypeClasses #-}
module Milkshake.MonitorSpec (spec) where

import RIO
import RIO.Directory (canonicalizePath)
import RIO.File (writeBinaryFile)

import Test.Hspec
import Util (runInTmp)

import Milkshake.Monitor

data Env = Env
    { _watchManager :: WatchManager
    , _channel      :: Chan Event
    , _logger       :: LogFunc
    }

instance HasWatchManager Env where
    watchManager = lens _watchManager (\e m -> e { _watchManager = m })

instance HasLogFunc Env where
    logFuncL = lens _logger (\e l -> e { _logger = l })

instance HasEventChannel Env Event where
    eventChannel = lens _channel (\e c -> e { _channel = c })

runEnv :: MonadUnliftIO m => RIO Env a -> m a
runEnv action = do
    logOptions <- logOptionsHandle stderr True
    withLogFunc logOptions (\logFunc -> do
        withWatchManager (\wm -> do
            ch <- newChan
            let env = Env wm ch logFunc
            runRIO env action))

spec :: Spec
spec = describe "Monitor" $ do
    it "monitors file creation" $ runInTmp [] $ do
        signal <- runEnv $ do
                chan <- view eventChannel
                stop <- monitor [(["./*"], return)]
                writeBinaryFile "test.txt" mempty
                signal <- timeout 1000 $ readChan chan
                stop
                return signal
        abs_filename <- canonicalizePath "./test.txt"
        signal `shouldSatisfy` \case
            Just (Added path _ _) -> path == abs_filename
            _                     -> False
-- ~\~ end
