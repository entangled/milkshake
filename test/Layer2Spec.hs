-- ~\~ language=Haskell filename=test/Layer2Spec.hs
-- ~\~ begin <<lit/index.md|test/Layer2Spec.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Layer2Spec (spec) where

import RIO
import qualified RIO.Text as T
import Test.Hspec
import qualified RIO.Map as M

import Milkshake.Data (Trigger(..), Rule(..), Action(..), Target(..), Generator)
import Milkshake.Run (enter)
import Dhall (auto, input, FromDhall, Decoder, constructor, union, list)

import Development.Shake (shake, shakeOptions, ShakeOptions(..), Verbosity(..))
import Util (runInTmp)

data Config = Config
    { rules :: M.Map Text Generator
    , triggers :: [Trigger]
    , actions :: [Action]
    , includes :: [Target] }
    deriving (Generic)

instance Semigroup Config where
    a <> b = Config
        { rules = (rules a) <> (rules b)
        , triggers = (triggers a) <> (triggers b)
        , actions = (actions a) <> (actions b)
        , includes = (includes a) <> (includes b)
        }

instance Monoid Config where
    mempty = Config
        { rules = mempty
        , triggers = mempty
        , actions = mempty
        , includes = mempty }

data Stmt
    = StmtAction Action
    | StmtRule Rule
    | StmtTrigger Trigger
    | StmtInclude Target

stmt :: Decoder Stmt
stmt = union (
       (StmtAction  <$> constructor "Action" auto)
    <> (StmtRule    <$> constructor "Rule" auto)
    <> (StmtTrigger <$> constructor "Trigger" auto)
    <> (StmtInclude <$> constructor "Include" auto))

readStmts :: (MonadIO m) => FilePath -> m [Stmt]
readStmts path = liftIO $ input (list stmt) (T.pack path)

stmtsToConfig :: [Stmt] -> Config
stmtsToConfig = foldMap toConfig
    where toConfig (StmtAction a) = mempty { actions = [a] }
          toConfig (StmtRule (Rule {..}))   = mempty { rules = M.singleton name gen }
          toConfig (StmtTrigger t) = mempty { triggers = [t] }
          toConfig (StmtInclude i) = mempty { includes = [i] }

fromTrigger :: Config -> Trigger -> Either Text Action
fromTrigger cfg Trigger{..} = case rule of
    Just r  -> Right $ Action target dependency (r target dependency)
    Nothing -> Left $ "No such rule: " <> name
    where rule = (rules cfg) M.!? name

data MilkShakeError
    = ConfigError Text
    deriving (Show, Eq)

instance Exception MilkShakeError

spec :: Spec
spec = describe "Layer2" $ do
    it "can load a configuration" $ runInTmp ["./test/Layer2/*"] $ do
        cfg <- stmtsToConfig <$> input (list stmt) "./test2.dhall"
        (actions cfg) `shouldSatisfy` any (\Action{..} -> target == [Phony "main"])
    it "can run all actions" $ runInTmp ["./test/Layer1/hello.c", "./test/Layer2/*"] $ do
        cfg <- stmtsToConfig <$> input (list stmt) "./test2.dhall"
        (actions cfg) `shouldSatisfy` any (\Action{..} -> target == [Phony "main"])
        case mapM (fromTrigger cfg) (triggers cfg) of
            Left e -> throwM (ConfigError e)
            Right as -> do
                let actionList = (actions cfg) <> as
                shake (shakeOptions { shakeVerbosity = Diagnostic }) (mapM_ enter actionList)
                result <- readFileUtf8 "out.txt"
                result `shouldBe` "Hello, World!\n"
-- ~\~ end
