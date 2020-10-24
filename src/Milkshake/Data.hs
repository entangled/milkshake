-- ~\~ language=Haskell filename=src/Milkshake/Data.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Data.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
{-# LANGUAGE DerivingStrategies,DerivingVia,DataKinds,UndecidableInstances #-}

module Milkshake.Data where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M

import Data.Monoid.Generic (GenericSemigroup(..), GenericMonoid(..))
import Dhall

-- ~\~ begin <<lit/index.md|haskell-types>>[0]
data Virtual = Virtual
    { name :: Text
    , exists :: Text
    , content :: Text }
    deriving (Generic, Show, Eq)

instance FromDhall Virtual
instance ToDhall Virtual

data Target
    = File Text
    | Generic Virtual
    | Phony Text
    deriving (Generic, Show, Eq)

instance FromDhall Target
instance ToDhall Target
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[1]
data Action = Action
    { target :: [ Target ]
    , dependency :: [ Target ]
    , script :: Maybe Text }
    deriving (Generic, Show)

instance FromDhall Action
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[2]
type Generator = [Target] -> [Target] -> Maybe Text

data Rule = Rule
    { name :: Text
    , gen :: Generator }
    deriving (Generic)

instance FromDhall Rule
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[3]
data Trigger = Trigger
    { name :: Text
    , target :: [ Target ]
    , dependency :: [ Target ] }
    deriving (Generic, Show)

instance FromDhall Trigger
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[4]
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
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[5]
data Config = Config
    { rules :: M.Map Text Generator
    , triggers :: [Trigger]
    , actions :: [Action]
    , includes :: [Target] }
    deriving (Generic)
    deriving Semigroup via GenericSemigroup Config
    deriving Monoid    via GenericMonoid Config

stmtsToConfig :: [Stmt] -> Config
stmtsToConfig = foldMap toConfig
    where toConfig (StmtAction a) = mempty { actions = [a] }
          toConfig (StmtRule (Rule {..}))   = mempty { rules = M.singleton name gen }
          toConfig (StmtTrigger t) = mempty { triggers = [t] }
          toConfig (StmtInclude i) = mempty { includes = [i] }
-- ~\~ end
-- ~\~ end
