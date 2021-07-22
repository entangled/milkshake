-- ~\~ language=Haskell filename=src/Milkshake/Monitor.hs
-- ~\~ begin <<docs/milkshake.md|src/Milkshake/Monitor.hs>>[0]
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
This module contains functionality that involves the interface with FSNotify.
 -}
module Milkshake.Monitor
    ( monitor, GlobList, FileEventHandler, Watch, StopListening
    , withWatchManager, Event(..), eventPath
    , WatchManager, HasWatchManager(..), HasEventChannel(..) ) where

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

class HasWatchManager env where
    watchManager :: Lens' env WatchManager

class HasEventChannel env event where
    eventChannel :: Lens' env (Chan event)

{-| Unlifted version of 'System.FSNotify.withManager'. -}
withWatchManager :: MonadUnliftIO m => (WatchManager -> m a) -> m a
withWatchManager callback = do
    withRunInIO (\run -> liftIO $ withManager (run . callback))

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
    stopActions <- withRunInIO (\run ->
        liftIO $ mapM
            (\dir -> watchDir wm dir (const True)
                (\ev -> run $ handler ev >>= writeChan chan)) dirList)
    return $ liftIO $ sequence_ stopActions

{-| Starts a number of watches, where each watch is specified by a list of
    glob-patterns and a handler that converts 'Event' to a message. Generated
    events are pushed to the given channel. Returns an IO action that will stop
    all of these watches.

    The glob-pattern is expanded such that all directories containing matching
    files are watched. In addition we also watch these directories if they're
    empty, so that we trigger on file creation events. -}
monitor :: ( MonadUnliftIO m, MonadReader env m, HasLogFunc env
           , HasWatchManager env, HasEventChannel env event )
        => [Watch m event] -> m (StopListening m)
monitor watches = do
    wm <- view watchManager
    ch <- view eventChannel
    stopActions <- mapM (setWatch wm ch) watches
    return $ sequence_ stopActions
-- ~\~ end
