-- ~\~ language=Haskell filename=src/Milkshake/Data.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Data.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Milkshake.Data where

import RIO
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
-- ~\~ end
