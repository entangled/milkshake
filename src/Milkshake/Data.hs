-- ~\~ language=Haskell filename=src/Milkshake/Data.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Data.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,DuplicateRecordFields,OverloadedLabels #-}
module Milkshake.Data where

import RIO
import Dhall

-- ~\~ begin <<lit/index.md|haskell-types>>[0]
data Content = Content
    { name :: Text
    , exists :: Text
    , hash :: Text }
    deriving (Generic, Show, Eq)

instance FromDhall Content
instance ToDhall Content

data Target
    = File Text
    | Generic Content
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
type Rule = [Target] -> [Target] -> Maybe Text
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
