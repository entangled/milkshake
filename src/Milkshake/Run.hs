-- ~\~ language=Haskell filename=src/Milkshake/Run.hs
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Run.hs>>[init]
{-# LANGUAGE DuplicateRecordFields,OverloadedLabels #-}
{-|
Contains functions to execute the script using Shake.
 -}
module Milkshake.Run where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M

import Development.Shake (shake, shakeOptions)
import qualified Development.Shake as Shake
import Milkshake.Data
    ( readStmts,
      stmtsToConfig,
      Action(..),
      Call(..),
      Config(..),
      Target(Phony, File) )
import Milkshake.Error ( MilkshakeError(..) )

{-| Get `FilePath` from `Target` -}
targetPath :: Target -> Maybe FilePath
targetPath (File path) = Just $ T.unpack path
targetPath _ = Nothing

{-| Checks if target is a file -}
isFileTarget :: Target -> Bool
isFileTarget (File _) = True
isFileTarget _        = False

{-| Create `Shake.Rules` from an `Action`.
 -}
enter :: Action -> Shake.Rules ()
-- ~\~ begin <<lit/milkshake.md|enter-action>>[init]
enter Action{ target = [File path], .. } =
    T.unpack path Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|enter-action>>[1]
enter Action { target = [Phony n], .. }
    | n == "main" = Shake.want $ mapMaybe targetPath dependency
    -- ~\~ begin <<lit/milkshake.md|enter-phony>>[init]
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

{-| Helper function that creates a `Shake.Action ()` from a scriptlet. -}
runScript :: Text -> Shake.Action ()
runScript = mapM_ (Shake.cmd_ Shake.Shell) . lines . T.unpack
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Run.hs>>[1]
{-| Given a `Config` and a `Call`, creates an `Action`. -}
fromCall :: Config -> Call -> Either MilkshakeError Action
fromCall cfg Call{..} = case rule of
    Just r  -> Right $ Action target dependency (r target dependency)
    Nothing -> Left $ ConfigError $ "No such rule: " <> name
    where rule = rules cfg M.!? name

{-| Looks for actions that are immediately runnable. These are the plain
  `Action` statements, as well as calls to rules. The calls are expanded
  into actions, and a list of all actions is returned. -}
immediateActions :: Config -> Either MilkshakeError [Action]
immediateActions cfg@Config{..} = do
    triggered <- mapM (fromCall cfg) triggers
    return $ actions <> triggered

{-| Recursively loads include statements, until no includes are left. -}
loadIncludes :: (MonadThrow m, MonadIO m) => Config -> m Config
loadIncludes cfg@Config{includes=[]} = return cfg
loadIncludes cfg@Config{includes} = do
    actions <- either throwM return $ immediateActions cfg
    liftIO $ shake shakeOptions (mapM_ enter actions >> Shake.want includes)
    stmts <- foldMapM readStmts (map (T.pack . ("./" <>)) includes)
    loadIncludes $ cfg {includes = mempty} <> stmtsToConfig stmts
-- ~\~ end
