-- ~\~ language=Haskell filename=src/Milkshake/Monitor.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Monitor.hs>>[0]
module Milkshake.Monitor
    ( monitor, GlobList, FileEventHandler, Watch, StopListening
    , withWatchManager, Event(..), eventPath ) where

import RIO
import RIO.List (nub)
import RIO.FilePath (takeDirectory)
import RIO.Directory (canonicalizePath)
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
    where search = mconcat <$> mapM (glob . T.unpack) globs
          canonicalize = mapM canonicalizePath

setWatch :: MonadUnliftIO m => WatchManager -> Chan event 
                            -> Watch m event -> m (StopListening m)
setWatch wm chan (globs, handler) = do
    fileList <- globCanon globs
    let dirList = nub $ map takeDirectory fileList
    stopActions <- withRunInIO $ (\run ->
        liftIO $ mapM
            (\dir -> watchDir wm dir (const True)
                (\ev -> run $ handler ev >>= writeChan chan)) dirList)
    return $ liftIO $ sequence_ stopActions

monitor :: MonadUnliftIO m => WatchManager -> Chan event 
                           -> [Watch m event] -> m (StopListening m)
monitor wm chan watches = do
    stopActions <- mapM (setWatch wm chan) watches
    return $ sequence_ stopActions
-- ~\~ end
