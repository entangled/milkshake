-- ~\~ language=Haskell filename=src/Milkshake/Data.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Data.hs>>[0]
{-# LANGUAGE DuplicateRecordFields,OverloadedLabels #-}
{-# LANGUAGE DerivingStrategies,DerivingVia,DataKinds,UndecidableInstances #-}

module Milkshake.Data where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as M

import Data.Monoid.Generic (GenericSemigroup(..), GenericMonoid(..))
import Dhall (FromDhall, ToDhall, Decoder, union, constructor, auto, input, list)

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
{-| An `Action` is a node in our workflow. -}
data Action = Action
    { target :: [ Target ]
    , dependency :: [ Target ]
    , script :: Maybe Text
    } deriving (Generic, Show)

instance FromDhall Action
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[2]
type Generator = [Target] -> [Target] -> Maybe Text

{-| A `Rule` is a parametric `Action`. Given a list of targets and dependencies,
    the generator creates the corresponding script. -}
data Rule = Rule
    { name :: Text                  -- ^ a unique name for this rule
    , gen :: Generator              -- ^ the generator function for the script
    } deriving (Generic)

instance FromDhall Rule
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[3]
{-| The `Trigger` is like a function call, where the `Rule` is the function
    and `target` and `dependecy` are the arguments. -}
data Trigger = Trigger
    { name :: Text                  -- ^ the name of the rule to trigger
    , target :: [ Target ]          -- ^ the targets
    , dependency :: [ Target ]      -- ^ the dependencies
    } deriving (Generic, Show)

instance FromDhall Trigger
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[4]
{-| The 'Stmt' type encodes lines in a Milkshake configuration. -}
data Stmt
    = StmtAction Action         {-^ -}
    | StmtRule Rule
    | StmtTrigger Trigger
    | StmtInclude FilePath
    | StmtMain [FilePath]
    -- ~\~ begin <<lit/index.md|stmt-type>>[0]
    | StmtWatch Watch
    -- ~\~ end

{-| To decode a list of Milkshake statements from the Dhall configuration
    use this decoder.

  >>> input (list stmt) "(entangled.dhall).milkshake"
  -}
stmt :: Decoder Stmt
stmt = union (
       (StmtAction  <$> constructor "Action" auto)
    <> (StmtRule    <$> constructor "Rule" auto)
    <> (StmtTrigger <$> constructor "Trigger" auto)
    <> (StmtInclude <$> constructor "Include" auto)
    <> (StmtMain    <$> constructor "Main" auto)
    -- ~\~ begin <<lit/index.md|stmt-decoder>>[0]
    <> (StmtWatch   <$> constructor "Watch" auto)
    -- ~\~ end
    )

readStmts :: (MonadIO m) => FilePath -> m [Stmt]
readStmts path = liftIO $ input (list stmt) (T.pack path)
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[5]
{-| Transposed data record of a list of `Stmt`. -}
data Config = Config
    { rules      :: M.Map Text Generator
    , triggers   :: [Trigger]
    , actions    :: [Action]
    , includes   :: [FilePath]
    , mainTarget :: [FilePath]
    , watches    :: [Watch] }
    deriving (Generic)
    deriving Semigroup via GenericSemigroup Config
    deriving Monoid    via GenericMonoid Config

{-| Groups a list of 'Stmt' into a 'Config' record. -}
stmtsToConfig :: [Stmt] -> Config
stmtsToConfig = foldMap toConfig
    where toConfig (StmtAction a) = mempty { actions = [a] }
          toConfig (StmtRule Rule {..})   = mempty { rules = M.singleton name gen }
          toConfig (StmtTrigger t) = mempty { triggers = [t] }
          toConfig (StmtInclude i) = mempty { includes = [i] }
          toConfig (StmtMain m) = mempty { mainTarget = m }
          toConfig (StmtWatch w) = mempty { watches = [w] }

readConfig :: (MonadIO m) => FilePath -> m Config
readConfig f = stmtsToConfig <$> readStmts f
-- ~\~ end
-- ~\~ begin <<lit/index.md|haskell-types>>[6]
data Watch = Watch
    { paths :: [Text]
    , target :: Target
    } deriving (Generic)

instance FromDhall Watch
-- ~\~ end
-- ~\~ end
