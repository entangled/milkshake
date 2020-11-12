-- ~\~ language=Haskell filename=src/Milkshake/Monitor.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Monitor.hs>>[0]
module Milkshake.Monitor
    ( monitor, GlobList, FileEventHandler, Watch, StopListening
    , withWatchManager, Event(..), eventPath ) where

import RIO
import RIO.List (nub)
import RIO.FilePath (takeDirectory)
import RIO.Directory (canonicalizePath, doesDirectoryExist)
import qualified RIO.Text as T

import System.FilePath.Glob (glob)
import System.FSNotify (withManager, WatchManager, Event(..), watchDir, eventPath)

type GlobList = [Text]
type FileEventHandler m event = Event -> m event
type Watch m event = (GlobList, FileEventHandler m event)
type StopListening m = m ()

withWatchManager :: MonadUnliftIO m => (WatchManager -> m a) -> m a
withWatchManager callback = do
    withRunInIO $ (\run -> liftIO $ withManager (run . callback))

globCanon :: MonadIO m => [Text] -> m [FilePath]
globCanon globs = liftIO $ search >>= canonicalize
    where search = do
            files <- mconcat <$> mapM (glob . T.unpack) globs
            parents <- mconcat <$> mapM (glob . takeDirectory . T.unpack) globs
            dirs <- filterM doesDirectoryExist parents
            return $ nub $ dirs <> map takeDirectory files
          canonicalize = mapM canonicalizePath

setWatch :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
         => WatchManager -> Chan event 
         -> Watch m event -> m (StopListening m)
setWatch wm chan (globs, handler) = do
    dirList <- globCanon globs
    logDebug $ display $ "watching: " <> tshow dirList
    stopActions <- withRunInIO $ (\run ->
        liftIO $ mapM
            (\dir -> watchDir wm dir (const True)
                (\ev -> run $ handler ev >>= writeChan chan)) dirList)
    return $ liftIO $ sequence_ stopActions

monitor :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
        => WatchManager -> Chan event 
        -> [Watch m event] -> m (StopListening m)
monitor wm chan watches = do
    stopActions <- mapM (setWatch wm chan) watches
    return $ sequence_ stopActions
-- ~\~ end
