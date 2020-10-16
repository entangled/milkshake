-- ~\~ language=Haskell filename=src/Milkshake/Run.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Run.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Milkshake.Run where

import RIO
import qualified RIO.Text as T
import qualified Development.Shake as Shake
import Milkshake.Data

targetPath :: Target -> Maybe FilePath
targetPath (File path) = Just $ T.unpack path
targetPath _ = Nothing

enter :: Action -> Shake.Rules ()
enter Action{ target = [File path], .. } =
    (T.unpack path) Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
enter Action { target = [Phony n], .. }
    | n == "main" = Shake.want $ mapMaybe targetPath dependency
    | otherwise   = mempty
enter _ = mempty

runScript :: Text -> Shake.Action ()
runScript = mapM_ (Shake.cmd_ Shake.Shell) . lines . T.unpack
-- ~\~ end
