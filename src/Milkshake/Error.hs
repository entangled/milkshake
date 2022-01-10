-- ~\~ language=Haskell filename=src/Milkshake/Error.hs
-- ~\~ begin <<lit/milkshake.md|src/Milkshake/Error.hs>>[0]
{-| Everything related to error handling in Milkshake -}
module Milkshake.Error (MilkshakeError(..)) where

import RIO

{-| Internal Error type. -}
newtype MilkshakeError
    = ConfigError Text
    deriving (Show, Eq)

instance Exception MilkshakeError
-- ~\~ end
