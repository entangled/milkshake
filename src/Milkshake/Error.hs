-- ~\~ language=Haskell filename=src/Milkshake/Error.hs
-- ~\~ begin <<lit/index.md|src/Milkshake/Error.hs>>[0]
module Milkshake.Error (MilkshakeError(..)) where

import RIO

data MilkshakeError
    = ConfigError Text
    deriving (Show, Eq)

instance Exception MilkshakeError
-- ~\~ end
