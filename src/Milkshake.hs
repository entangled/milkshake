-- ~\~ language=Haskell filename=src/Milkshake.hs
-- ~\~ begin <<docs/milkshake.md|src/Milkshake.hs>>[0]
{-|
Module     : Milkshake
Copyright  : (c) Netherlands eScience Center, 2021
                 Johan Hidding, 2021
License    : Apache-2
Maintainer : j.hidding@esciencenter.nl
Stability  : experimental

Experimental prototype: combine Dhall, Shake and FSNotify to create a
generic build system that triggers on filesystem events.
 -}
module Milkshake ( Config(..)
                 , readConfig
                 , loadIncludes
                 , WatchManager
                 , Event
                 , monitor
                 , HasWatchManager(..)
                 , HasEventChannel(..)
                 , withWatchManager
                 , shake
                 , shakeOptions
                 , want
                 , enter
                 , immediateActions
                 ) where

import Milkshake.Data ( readConfig, Config(..) )
import Milkshake.Run ( enter, loadIncludes, immediateActions )
import Milkshake.Monitor ( WatchManager, Event, monitor, HasWatchManager(..), HasEventChannel(..), withWatchManager )

import Development.Shake (shake, shakeOptions, want)
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|src/Milkshake.hs>>[0]
{-|
Module     : Milkshake
Copyright  : (c) Netherlands eScience Center, 2021
                 Johan Hidding, 2021
License    : Apache-2
Maintainer : j.hidding@esciencenter.nl
Stability  : experimental

Experimental prototype: combine Dhall, Shake and FSNotify to create a
generic build system that triggers on filesystem events.
 -}
module Milkshake ( Config(..)
                 , readConfig
                 , loadIncludes
                 , WatchManager
                 , Event
                 , monitor
                 , HasWatchManager(..)
                 , HasEventChannel(..)
                 , withWatchManager
                 , shake
                 , shakeOptions
                 , want
                 , enter
                 , immediateActions
                 ) where

import Milkshake.Data ( readConfig, Config(..) )
import Milkshake.Run ( enter, loadIncludes, immediateActions )
import Milkshake.Monitor ( WatchManager, Event, monitor, HasWatchManager(..), HasEventChannel(..), withWatchManager )

import Development.Shake (shake, shakeOptions, want)
-- ~\~ end
