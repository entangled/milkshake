-- ~\~ language=Haskell filename=src/Milkshake/Run.hs
-- ~\~ begin <<docs/milkshake.md|src/Milkshake/Run.hs>>[0]
{-# LANGUAGE DuplicateRecordFields,OverloadedLabels #-}
{-|
Contains functions to execute the workflows using Shake.
 -}
module Milkshake.Run where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M

import Development.Shake (shake, shakeOptions)
import qualified Development.Shake as Shake
import Milkshake.Data
import Milkshake.Error

targetPath :: Target -> Maybe FilePath
targetPath (File path) = Just $ T.unpack path
targetPath _ = Nothing

isFileTarget :: Target -> Bool
isFileTarget (File _) = True
isFileTarget _        = False

enter :: Action -> Shake.Rules ()
-- ~\~ begin <<docs/milkshake.md|enter-action>>[0]
enter Action{ target = [File path], .. } =
    T.unpack path Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
-- ~\~ end
-- ~\~ begin <<docs/milkshake.md|enter-action>>[1]
enter Action { target = [Phony n], .. }
    | n == "main" = Shake.want $ mapMaybe targetPath dependency
    -- ~\~ begin <<docs/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
    -- ~\~ begin <<lit/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|enter-action>>[0]
enter Action{ target = [File path], .. } =
    T.unpack path Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|enter-action>>[1]
enter Action { target = [Phony n], .. }
    | n == "main" = Shake.want $ mapMaybe targetPath dependency
    -- ~\~ begin <<docs/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
    -- ~\~ begin <<lit/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
-- ~\~ end
enter Action { target = ts@(_:_), .. }
    | all isFileTarget ts =
          tgtPaths Shake.&%> \_ -> do
              Shake.need $ mapMaybe targetPath dependency
              mapM_ runScript script
    | otherwise           = mempty
    where tgtPaths = mapMaybe targetPath ts
enter _ = mempty

runScript :: Text -> Shake.Action ()
runScript = mapM_ (Shake.cmd_ Shake.Shell) . lines . T.unpack
-- ~\~ end
-- ~\~ begin <<docs/milkshake.md|src/Milkshake/Run.hs>>[1]
fromTrigger :: Config -> Trigger -> Either MilkshakeError Action
fromTrigger cfg Trigger{..} = case rule of
    Just r  -> Right $ Action target dependency (r target dependency)
    Nothing -> Left $ ConfigError $ "No such rule: " <> name
    where rule = rules cfg M.!? name

immediateActions :: Config -> Either MilkshakeError [Action]
immediateActions cfg@Config{..} = do
    triggered <- mapM (fromTrigger cfg) triggers
    return $ actions <> triggered

loadIncludes :: (MonadThrow m, MonadIO m) => Config -> m Config
loadIncludes cfg@Config{includes=[]} = return cfg
loadIncludes cfg@Config{includes} = do
    actions <- either throwM return $ immediateActions cfg
    liftIO $ shake shakeOptions (mapM_ enter actions >> Shake.want includes)
    stmts <- foldMapM readStmts (map ("./" <>) includes)
    loadIncludes $ cfg {includes = mempty} <> stmtsToConfig stmts
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Run.hs>>[0]
{-# LANGUAGE DuplicateRecordFields,OverloadedLabels #-}
{-|
Contains functions to execute the workflows using Shake.
 -}
module Milkshake.Run where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M

import Development.Shake (shake, shakeOptions)
import qualified Development.Shake as Shake
import Milkshake.Data
import Milkshake.Error

targetPath :: Target -> Maybe FilePath
targetPath (File path) = Just $ T.unpack path
targetPath _ = Nothing

isFileTarget :: Target -> Bool
isFileTarget (File _) = True
isFileTarget _        = False

enter :: Action -> Shake.Rules ()
-- ~\~ begin <<docs/milkshake.md|enter-action>>[0]
enter Action{ target = [File path], .. } =
    T.unpack path Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
-- ~\~ end
-- ~\~ begin <<docs/milkshake.md|enter-action>>[1]
enter Action { target = [Phony n], .. }
    | n == "main" = Shake.want $ mapMaybe targetPath dependency
    -- ~\~ begin <<docs/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
    -- ~\~ begin <<lit/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|enter-action>>[0]
enter Action{ target = [File path], .. } =
    T.unpack path Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|enter-action>>[1]
enter Action { target = [Phony n], .. }
    | n == "main" = Shake.want $ mapMaybe targetPath dependency
    -- ~\~ begin <<docs/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
    -- ~\~ begin <<lit/milkshake.md|enter-phony>>[0]
    | otherwise   = Shake.phony (T.unpack n) $ do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
    -- ~\~ end
-- ~\~ end
enter Action { target = ts@(_:_), .. }
    | all isFileTarget ts =
          tgtPaths Shake.&%> \_ -> do
              Shake.need $ mapMaybe targetPath dependency
              mapM_ runScript script
    | otherwise           = mempty
    where tgtPaths = mapMaybe targetPath ts
enter _ = mempty

runScript :: Text -> Shake.Action ()
runScript = mapM_ (Shake.cmd_ Shake.Shell) . lines . T.unpack
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Run.hs>>[1]
fromTrigger :: Config -> Trigger -> Either MilkshakeError Action
fromTrigger cfg Trigger{..} = case rule of
    Just r  -> Right $ Action target dependency (r target dependency)
    Nothing -> Left $ ConfigError $ "No such rule: " <> name
    where rule = rules cfg M.!? name

immediateActions :: Config -> Either MilkshakeError [Action]
immediateActions cfg@Config{..} = do
    triggered <- mapM (fromTrigger cfg) triggers
    return $ actions <> triggered

loadIncludes :: (MonadThrow m, MonadIO m) => Config -> m Config
loadIncludes cfg@Config{includes=[]} = return cfg
loadIncludes cfg@Config{includes} = do
    actions <- either throwM return $ immediateActions cfg
    liftIO $ shake shakeOptions (mapM_ enter actions >> Shake.want includes)
    stmts <- foldMapM readStmts (map ("./" <>) includes)
    loadIncludes $ cfg {includes = mempty} <> stmtsToConfig stmts
-- ~\~ end
