-- ~\~ language=Haskell filename=test/Layer1Spec.hs
-- ~\~ begin <<lit/index.md|test/Layer1Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Layer1Spec (spec) where

import RIO
import qualified RIO.Text as T
import RIO.Directory (getCurrentDirectory, setCurrentDirectory)
import Test.Hspec
import Recipe
import Dhall

import qualified Development.Shake as Shake
import Development.Shake (shake, shakeOptions)

targetPath :: Target -> Maybe FilePath
targetPath (File path) = Just $ T.unpack path
targetPath _ = Nothing

enter :: Action -> Shake.Rules ()
enter Action{ target = [File path], .. } =
    (T.unpack path) Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
enter Action { target = [Main], .. } =
    Shake.want $ mapMaybe targetPath dependency
enter _ = mempty

runScript :: Text -> Shake.Action ()
runScript = mapM_ (Shake.cmd_ Shake.Shell) . lines . T.unpack

spec :: Spec
spec = describe "Recipe" $ do
    it "can load a list of actions" $ do
        actionList <- input auto "./test/Layer1/test1.dhall" :: IO [Action]
        actionList `shouldSatisfy` any (\a -> target a == [Main])
    it "can run a list of actions" $ do
        actionList <- input auto "./test/Layer1/test1.dhall" :: IO [Action]
        cwd <- getCurrentDirectory
        setCurrentDirectory "./test/Layer1"
        shake shakeOptions (mapM_ enter actionList)
        setCurrentDirectory cwd
-- ~\~ end
