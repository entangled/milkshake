-- ~\~ language=Haskell filename=src/Milkshake.hs
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

= Why
Use this if you need to embed a configurable build system in your application.
You can use `Milkshake` to read Dhall files that specify custom sets of actions,
plug your built-in functionality and run the workflow using `Shake`.

We also give an executable @milkshake@ that you can run directly.

= Nomenclature
* Target:

    * File target: a target identified by a file.
    * Virtual target: a target that acts like a file, but could be anything, e.g. a database entry.
    * Phony target: a target that is not.

* Action: an /action/ is a scriptlet with fixed input and output targets.
* Rule: a /rule/ acts like a function, taking a list of targets and a list of
dependencies, generating an /action/.
* Include: an /include/ statement specifies a target that should be a valid Milkshake
script, this script is then loaded and included into the current set of statements.
Include statements are loaded recursively, going a bit against the grain of `Shake`.
* Watches: while Milkshake is running, files can be watched for changes, triggering rebuilds.

= Tutorial
Milkshake uses the Dhall language to specify dependencies between actions. In the most basic operation, we have `Target` and `Action`. A `Target` is usually one or more files, while an `Action` is a shell script that would generate said `Target`, after satisfying a set of dependencies (also `Target`).

The resulting configuration is a list of `Action`. Since we will add other forms of *statements* later on, we use some Dhall functions to write out actions. For example, to create a file `hello.txt` and consequently put it in all caps, could be done as follows:

@
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in [ ms.fileAction "hello.txt" ([] : List Text) ''
         echo "Hello, World" \> hello.txt
         ''
   , ms.fileAction \"HELLO.TXT\" [\"hello.txt\"] ''
         tr a-z A-Z \< hello.txt \> HELLO.TXT
         ''
   , ms.mainAction [\"HELLO.TXT\"]
]
@

== Rules
Because it is not so nice to write out every action explicitely, we can define rules. A rule is a function from input targets to output targets. A rule can be triggered by calling it:

@
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall
let Text\/concat = https://prelude.dhall-lang.org/Text/concat

in [ ms.fileAction "secret.txt" ([] : List Text)
         ''
         echo "Uryyb, Jbeyq!" > secret.txt
         ''
   , ms.fileRule "rot13" (\\(tgt : Text) -> \\(deps : List Text) ->
         ''
         tr a-zA-Z n-za-mN-ZA-M \< ${Text\/concat deps} \> ${tgt}
         '')
   , ms.fileCall "rot13" "message.txt" ["secret.txt"]
   , ms.mainAction ["message.txt"]
]
@

== Includes
Includes allow one to incorporate the contents of one Milkshake file into an other. The nice thing is that the include can be the result of an action. As an example we have here a template that takes a number as an argument:

@
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in \(x : Natural) ->
    [ ms.fileAction "answer.txt" ([] : List Text)
        ''
        echo "${Natural\/show x}" > answer.txt
        ''
    ]
@

The main file imports the generated include

@
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in  [ ms.fileAction "include.dhall" ([] : List Text)
        ''
        dhall \<\<\< "./template.dhall 42" \> include.dhall
        ''
    , ms.include "include.dhall"
    , ms.main ["answer.txt"]
    ]
@

== Watches
Milkshake can set a number of watches to trigger a file to be built.

@
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in [ ms.fileRule "rot13" (\\(tgt : Text) -> \\(deps : List Text) ->
         ''
         tr a-zA-Z n-za-mN-ZA-M \< ${Text/concat deps} \> ${tgt}
         '')
   , ms.fileCall "rot13" "secret.txt" ["message.txt"]
   , ms.watch ["message.txt"] (ms.Target.File "secret.txt")
]
@
 -}
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|src/Milkshake.hs>>[1]
module Milkshake ( Config(..)
                 , Stmt(..)
                 , stmtsToConfig
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

import Milkshake.Data ( readConfig, Config(..), Stmt(..), stmtsToConfig )
import Milkshake.Run ( enter, loadIncludes, immediateActions )
import Milkshake.Monitor ( WatchManager, Event, monitor, HasWatchManager(..), HasEventChannel(..), withWatchManager )

import Development.Shake (shake, shakeOptions, want)
-- ~\~ end
