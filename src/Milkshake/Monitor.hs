-- ~\~ language=Haskell filename=src/Milkshake/Monitor.hs
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Monitor.hs>>[0]
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
This module contains functionality that involves the interface with FSNotify.
The only important function here is `monitor`, next to some reexports from FSNotify.
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

{-| File paths are expanded with `System.FilePath.Glob`. Input to `Watch` functionality,
  i.e. a list of file patterns should follow this type. -}
type GlobList = [Text]
{-| An event handler, taking an `System.FSNotify.Event` and returning some internal
  message type. -}
type FileEventHandler m event = Event -> m event
{-| A `Watch` is a list of file patterns to watch and a corresponding `FileEventHandler`
 -}
type Watch m event = (GlobList, FileEventHandler m event)
{-| Every time we set a watch, we are given a function that, when called, unsets the
    watch. -}
type StopListening m = m ()

{-| RIO class for obtaining the `System.FSNotify.WatchManager`.
 -}
class HasWatchManager env where
    watchManager :: Lens' env WatchManager

{-| RIO class for obtaining the event channel to which event messages are pushed.
 -}
class HasEventChannel env event where
    eventChannel :: Lens' env (Chan event)

{-| Unlifted version of 'System.FSNotify.withManager'. -}
withWatchManager :: MonadUnliftIO m => (WatchManager -> m a) -> m a
withWatchManager callback = do
    withRunInIO (\run -> liftIO $ withManager (run . callback))

{-| Expand a glob string into all realised paths, and return a list with
  unique containing directories. Those are the ones we set watches on.
  -}
globCanon :: MonadIO m => [Text] -> m [FilePath]
globCanon globs = liftIO $ nub <$> (search >>= canonicalize)
    where search = do
            files <- mconcat <$> mapM (glob . T.unpack) globs
            parents <- mconcat <$> mapM (glob . takeDirectory . T.unpack) globs
            dirs <- filterM doesDirectoryExist parents
            return $ dirs <> map takeDirectory files
          canonicalize = mapM canonicalizePath

{-| Set a watch. -}
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
