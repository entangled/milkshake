-- ~\~ language=Haskell filename=src/Milkshake/Error.hs
-- ~\~ begin <<docs/milkshake.md|src/Milkshake/Error.hs>>[0]
module Milkshake.Error (MilkshakeError(..)) where

import RIO

newtype MilkshakeError
    = ConfigError Text
    deriving (Show, Eq)

instance Exception MilkshakeError
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Error.hs>>[0]
module Milkshake.Error (MilkshakeError(..)) where

import RIO

newtype MilkshakeError
    = ConfigError Text
    deriving (Show, Eq)

instance Exception MilkshakeError
-- ~\~ end
