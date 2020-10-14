---
title: Milkshake
subtitle: A filesystem triggered build system
author: Johan Hidding
---

# Build systems
A normal build system is mostly file-system based. Every task is designated a target file and a list of dependencies. When a file somewhere down the line is updated, you rerun `make` and only the affected part of the dependency tree is updated, as shown in Figure {@fig:dependency-graph}.

``` {.dot #dependency-graph .shake}
digraph G {
    node [shape = "box", style = "rounded, filled", color = "#cccccc"]
    a -> b;
    b -> e;
    c [color = "#aaccff", label = < c <br /><font point-size="8">(changed)</font> >];
    e [color = "#aaccff"];
    g [color = "#aaccff", label = < g <br /><font point-size="8">(target)</font> >];
    c -> e;
    d -> f;
    e -> g;
    f -> g;
}
```

This simple principle can be written down in a `Makefile`

``` {.make #example-makefile}
b: a
        <<create-b-from-a>>

e: b c
        <<create-e-from-b-and-c>>

f: d
        <<create-d-from-f>>

g: e f
        <<create-g-from-e-and-f>>
```

All dependencies in Figure {@fig:dependency-graph} are listed in `<<example-makefile>>` including the recipes for each step (in the shape of some shell script). Since we're committed to a *Dhall* configuration file, we need to write down a similar structure in the Dhall language.

## Cases / Persona

### Me
A theorist involved in modelling (astro)physical systems.

Needs:
- A model, a code: Entangled handles this beautifully.
- Building the code: we use a `Makefile`, or alternatively `meson` to coordinate building C++. For new projects Rust may be a better choice and building is handled by `cargo`. The availability of build systems in modern languages obliviates the need for complicated build system functionality in Entangled.
- Running the code:
    - traditionally in bash
    - more complicated workflows: link C++ to Python interface, build a notebook
    - Can we encapsulate some of the workflow in the text, such that the analysis merges fluently with the text?
- Visualisation:
    - gnuplot
    - python + matplotlib
    - paraview, is it scriptable?
    - blender

## A minimal build system
By no means should we aim for the level of features found in GNU Make. A more approachable target is *Ninja*. We will still discuss these features using Make syntax.

### Variables
Variables and string interpolation are supported by Dhall, so we won't have to. In Make you would say

``` {.make #compiling-c-make}
cflags := -Wall

%.o: %.c
        gcc $(cflags) -c $< -o $@
```

In Dhall this would be achieved by,

``` {.dhall #compiling-c-dhall}
let cflags = "-Wall"
in { targets = [
        { target = "hello.o", dependencies = ["hello.c"], script = ''
              gcc ${cflags} -c hello.c -o hello.o''
        } ]
   }
```

### Rules
A rule is a generic recipe for a set of specific targets. In the example above, there is a rule that describes how to compile a c-source file into an object file. It is a short-hand for every c-file in the distribution. We could keep the Make syntax for describing a pattern for the rule, but for the expansion we have could also use a Dhall function. Since we're using Shake however, we can also have Shake do the expansion. 

``` {.haskell #shake-snippet}
do
   "_build//*.o" %> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        need [c]
        cmd_ "gcc -c" [c] "-o" [out]
```

Shake supports a pattern language to match targets against a defined rule. There is no generic method of getting to the patterned name of the dependency. We could use regular expressions to achieve the same effect; not ideal but very powerful. Unfortunately, the state of regex support in Haskell is abismal. A rule definition for compiling c-files could look something like:

``` {.dhall #rule-definition}
let buildDir = "build"
let cflags = "-Wall"
let compileC = rule "${buildDir}/%.o" "%.c" 
                    \(out : Text) -> \(inp : Text) ->
        { target = out, dependencies = [ inp ], script = ''
          gcc ${cflags} -c ${inp} -o ${out}'' }

in {
   rules = [ compileC ]
}
```

On the other hand, we could just defer these more complicated cases to a real build system. We wouldn't need to support rules and regexes or anything. This is why we have `make`, `cmake`, `autotools`, `meson`, `ninja`, `jam` etc. right? Not to forget about workflow engines/standards like `snakemake`, `cwl`, `luigi`, `pegasus`, `airflow`, `taverna`, etc. Let's not go down that road! (and then he did)

### Phony targets
Phony targets are targets that are not identified by any file that will be created, but rather are an alias for one or more other targets.We can say any target starting with `#` is phony. These targets will acquire another meaning in the next section about Entangled, but I see no reason why this namespace shouldn't double up for phony targets and code fragments.

## Special to Entangled
In the case of *Entangled*, we have to track dependencies over *fragments of code* in addition to files.

We may specify a code-fragment by its identifier using `#` notation.

In the case of a generated target, the name will be a random generated identifier (UUID hex string), and values may be stored in the Entangled database.

### Oracle rules
We need the build system to work with information from the Entangled database. We may choose to have all actions be filesystem based. That would make the workflow inspectable. However, already in current use cases, we put code into fragments without attaching them to file content, and then evaluate that either in Jupyter or by JS code injection. To work with information from the database directly, we would need to use Oracle rules in Shake.

- [ ] (nice to have) Tab-completion on code block names
  
### Special targets
Some actions are triggered by events. By coupling a phony target to an event we can express the entire Entangled workflow in terms of the build system.

- `tangle` on changed file `<s>`
    - update database: `entangled insert -s <s>`
    - update target files: `entangled tangle -a`
    - clear orphans: `entangled clear-orphans`

- `stitch` on changed file `<t>`
    - update database: `entangled insert -t <t>`
    - update source files: `entangled stitch -a`
    - tangle covariant targets: `entangled tangle -a`

Given an up-to-date database we can ask Entangled for the existing dependencies. We already have the `list` command; now we should duplicate the `gcc -MM` kind of behaviour to generate a Makefile syntax dependency list on all markdown source and target files. Generating this information needs tracing back through the Markdown files in a way that is just as expensive as tangling or stitching is anyway. We could extend the functionality of `entangled list` to also list Markdown sources, slightly trivial, but still good to have.

- [ ] Implement `entangled list -s`
- [ ] Implement `entangled list -r` listing all named references

In an extended workflow, the `tangle` and `stitch` methods could also trigger compilation, publication etc.

### Process code blocks
When we need to process some code block, there are several options.

- Generate embedable content from the codeblock. Example: create SVG using Graphviz' Dot language. Example: create SVG plot with Gnuplot; bonus: depends on a model being compiled from tangled code. We have `entangled tangle -r <ref>` input, some script, and then (most often) an SVG or PNG file output, of which we need to know the name.
- Generate injectable content from the codeblock. Example that wouldn't work with previous method: generate table from data. We have `entangled tangle -r <ref>` input, some script, and then literal HTML or TeX output. This output needs to be injected into the final document by Pandoc. Either the output is stored into the Entangled database, or a target filename should be extractable using an `entangled` subcommand, probably hashing the input, build script and dependency hashes.
- Code blocks should list possible file dependencies. If data changes, output changes. In effect, a code block becomes a build-target itself. Do we have syntax for listing dependencies?

- [ ] Check pandoc accepted syntax for entering list of file names
- [ ] Check how we can make this work with `pandoc-fignos`

~~~markdown
 ``` {.gnuplot #plot-data depends=data.txt}
 set term svg
 set xrange [0:2*pi]
 plot sin(x), "data.txt" u 1:2 w lp
 ```

 ![Data compared with $sin(x)$.](fig/plot-data-sin.svg){#data-sin using=#plot-data}
~~~

Such an installation could then be configured by having a rule:

``` {.dhall #gnuplot-rule}
let gnuplot =
    { match = Match.Class "gnuplot"
    , script = \(out : Text) -> \(inp : Text) -> ''
          gnuplot ${inp} > ${out}''
    }

in  { scriptClasses = [ gnuplot ]
    }
```

The build system would then write the content of `#plot-sine` to a temporary file (with a consistent name, so we can do caching), run the script etc.

``` {.dhall file=test/basic/rules.dhall}
let Target : Type = < File : Text | Eval : Text >

let Rule : Type = { target : Text
                  , dependencies : List Text
                  , script : Text } 

let file = \(target : Text) -> \(deps : List Text) ->
           \(script : Text) ->
               { target = Target.File target
               , dependencies = deps
               , script = script }

let message = "Hello, World!"

in { shell = "bash"
   , rules = [
        file "hello.txt" ([] : List Text) ''
            echo "${message}" > {target}''
   ] }
```

### Bracketed spans
We can use bracketed spans to include generated files.

~~~markdown
 ``` {.bash #hello}
 echo "Hello, World!"
 ```

 [The output of the script.]{include=script-output.txt using=#hello}
~~~

# More formal approach
## Level 1
We start at the first level. This is where we get a list of things that need to be done and their implied dependencies.

We have a `Target` that describes an asset. A target can be a file or a database entry. A database entry should have a method of checking existence and content or a stable representation thereof (i.e. a hash). In this sense, a file is just a special case of a databse entry. We can read it using `cat` and get its modification time using `stat -c '%y'`. A `Target` can also be a named reference to an abstraction of an asset. Such an asset may not have an associated content, and would only be run if explicitly asked for. In Make this would be a `.PHONY` target.

``` {.dhall #milkshake-target}
let Content : Type =
    { exists : Text
    , hash : Text
    }

let Target : Type =
    < File : Text
    | Block : Text
    | Generic : Content
    | Phony : Text
    | Main
    >
```

To generate a target we have an `Action`. An action has one or more targets, a list of dependencies, and a script.

``` {.dhall #milkshake-action}
let Action : Type =
    { target : List Target
    , dependency : List Target
    , script : Optional Text
    }
```

This system of `Content`, `Target` and `Action` should suffice to describe every single instance of a build sequence.

## Level 2
The second level is when we can generate content, targets or actions based on function applications and patterns. The prime example we have seen before is that of a pattern rule in Make. We'd like to extend that to the use case of running scripts based on code-block content.

``` {.dhall #milkshake-rule}
let Rule =
    { match: Match
    , generator: List Target -> List Target -> Action
    }
```

In the GnuPlot example we saw how we could write down a script and link a figure to that script with `{using=#script-id}`.

To have this work, we need a pre-pass using Pandoc (see next section) and a query that finds figures that link to scripts. This should motivate the options we give for the `Match` data type.

``` {.dhall #milkshake-match}
let Match = < Glob : Text | Using : Text >
```

From a match we need to generate an action, by calling the `generator` member with a target and a list of dependencies. In the case of a `using` clause, these are generated by calling Pandoc with a special filter.

In the case of,

~~~markdown
 ``` {.bash #hello}
 echo "Hello, World!"
 ```

 [The output of the script.]{include=script-output.txt using=#hello}
~~~

the target would be `File "script-output.txt"` and dependencies `[ Block "hello" ]` and the generated action would amount to

``` {.dhall}
{ target = [ Target.File "script-output.txt" ]
, dependencies = [ Target.Block "hello" ]
, script = '''
    entangled tangle -r hello | bash
    '''
}
```

## Pre-scan
A lot of the targets will depend on a processed version of the input markdown. If a file is tangled from the markdown, it is a target. Also in the less trivial case discussed before, with the `using=` clause, we define a target. Entangled will have to generate a list of targets, dependencies and triggers.

``` {.dhall #milkshake-trigger}
let Trigger : Type =
    { target : List Target
    , dependencies : List Target
    , rule : Match }
```

There is the case where we'd like to trigger Entangled itself through watches.

# Incremental Programming
## HSpec and `hspec-discover`
``` {.haskell file=test/Spec.hs}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

``` {.haskell file=test/HSpec.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module HSpec (spec) where

import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "sanity" $ do
        it "1 + 1 == 2" $ do
            1 + 1 `shouldBe` (2 :: Int)
        it "2 + 2 != 5" $ do
            2 + 2 `shouldNotBe` 5
```

## First Layer
Given all of the previous considerations, we can start building Milkshake layer on layer. We start with the level 1 system, capable of taking action descriptions and putting them in Shake.

We add helper functions for defining a `file` action or the `main` action, which will serve as entry point.

``` {.dhall file=test/Layer1/schema.dhall}
let List/map = https://prelude.dhall-lang.org/v11.1.0/List/map
    sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

<<milkshake-target>>
<<milkshake-action>>

let file = \(target : Text) -> \(deps : List Text) -> \(script : Text) ->
   { target = [ Target.File target ]
   , dependency = List/map Text Target Target.File deps
   , script = Some script } : Action

let main = \(deps : List Text) ->
    { target = [ Target.Main ]
    , dependency = List/map Text Target Target.File deps
    , script = None Text } : Action

in { Target = Target
   , Action = Action
   , file = file
   , main = main }
```

## Example 1: compiling hello.c

``` {.dhall file=test/Layer1/test1.dhall}
let ms = ./schema.dhall

in [ ms.file "hello" [ "hello.c" ]
      ''
      gcc hello.c -o hello
      ''
   , ms.file "out.txt" [ "hello" ]
      ''
      ./hello > out.txt
      ''
   , ms.main [ "out.txt" ]
   ]
```

``` {.c file=test/Layer1/hello.c}
#include <stdio.h>
#include <stdlib.h>

int main() {
    printf("Hello, World!\n");
    return EXIT_SUCCESS;
}
```

### Loading the script

``` {.haskell file=src/Recipe.hs}
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
```

``` {.haskell file=test/Layer1Spec.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Layer1Spec (spec) where

import RIO
import qualified RIO.Text as T
import RIO.Directory (getCurrentDirectory, setCurrentDirectory)
import Test.Hspec
import Recipe
import Dhall

import qualified Development.Shake as Shake
import Development.Shake (shake, shakeOptions)

targetPath :: Target -> Maybe FilePath
targetPath (File path) = Just $ T.unpack path
targetPath _ = Nothing

enter :: Action -> Shake.Rules ()
enter Action{ target = [File path], .. } =
    (T.unpack path) Shake.%> \_ -> do
        Shake.need $ mapMaybe targetPath dependency
        mapM_ runScript script
enter Action { target = [Main], .. } =
    Shake.want $ mapMaybe targetPath dependency
enter _ = mempty

runScript :: Text -> Shake.Action ()
runScript = mapM_ (Shake.cmd_ Shake.Shell) . lines . T.unpack

spec :: Spec
spec = describe "Recipe" $ do
    it "can load a list of actions" $ do
        actionList <- input auto "./test/Layer1/test1.dhall" :: IO [Action]
        actionList `shouldSatisfy` any (\a -> target a == [Main])
    it "can run a list of actions" $ do
        actionList <- input auto "./test/Layer1/test1.dhall" :: IO [Action]
        cwd <- getCurrentDirectory
        setCurrentDirectory "./test/Layer1"
        shake shakeOptions (mapM_ enter actionList)
        setCurrentDirectory cwd
```
