-- ~\~ language=Haskell filename=src/Milkshake.hs
-- ~\~ begin <<lit/index.md|src/Milkshake.hs>>[0]
module Milkshake ( Config(..)
                 , readConfig
                 , loadIncludes
                 , WatchManager
                 , Event
                 , monitor
                 , HasWatchManager(..)
                 , HasEventChannel(..)
                 , withWatchManager
                 ) where

import Milkshake.Data ( readConfig, Config(..) )
import Milkshake.Run ( enter, loadIncludes, immediateActions )
import Milkshake.Monitor ( WatchManager, Event, monitor, HasWatchManager(..), HasEventChannel(..), withWatchManager )

import Development.Shake (shake, shakeOptions, want)
-- ~\~ end
