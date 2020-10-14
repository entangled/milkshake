-- ~\~ language=Haskell filename=src/Recipe.hs
-- ~\~ begin <<lit/index.md|src/Recipe.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Recipe where

import RIO
import Dhall

data Content = Content
    { exists :: Text
    , hash :: Text }
    deriving (Generic, Show, Eq)

instance FromDhall Content

data Target
    = File Text
    | Block Text
    | Generic Content
    | Phony Text
    | Main
    deriving (Generic, Show, Eq)

instance FromDhall Target

data Action = Action
    { target :: [ Target ]
    , dependency :: [ Target ]
    , script :: Maybe Text }
    deriving (Generic, Show)

instance FromDhall Action
-- ~\~ end
