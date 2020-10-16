-- ~\~ language=Haskell filename=test/Util.hs
-- ~\~ begin <<lit/index.md|test/Util.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Util (runInTmp) where

import RIO
import RIO.Directory (getCurrentDirectory, setCurrentDirectory, copyFile)
import System.FilePath.Glob (glob)
import RIO.FilePath ((</>), takeFileName)

runInTmp :: MonadUnliftIO m => [String] -> m () -> m ()
runInTmp cpy action = do
    paths <- liftIO $ foldMapM glob cpy
    withSystemTempDirectory "milkshake-" $ \tmp -> do
        cwd <- getCurrentDirectory
        mapM_ (\f -> copyFile f (tmp </> takeFileName f)) paths
        setCurrentDirectory tmp
        action
        setCurrentDirectory cwd
-- ~\~ end
