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

### Process code blocks
When we need to process some code block, there are several options.

- Generate embedable content from the codeblock. Example: create SVG using Graphviz' Dot language. Example: create SVG plot with Gnuplot; bonus: depends on a model being compiled from tangled code. We have `entangled tangle -r <ref>` input, some script, and then (most often) an SVG or PNG file output, of which we need to know the name.
- Generate injectable content from the codeblock. Example that wouldn't work with previous method: generate table from data. We have `entangled tangle -r <ref>` input, some script, and then literal HTML or TeX output. This output needs to be injected into the final document by Pandoc. Either the output is stored into the Entangled database, or a target filename should be extractable using an `entangled` subcommand, probably hashing the input, build script and dependency hashes.
- Code blocks should list possible file dependencies. If data changes, output changes. In effect, a code block becomes a build-target itself. Do we have syntax for listing dependencies?

- [ ] Check pandoc accepted syntax for entering list of file names
- [ ] Check how we can make this work with `pandoc-fignos`

~~~markdown
 ``` {.gnuplot #plot-sine target=plot.svg .figure}
 caption: The sine function.
 ---
 set term svg
 set xrange [0:2*pi]
 plot sin(x)
 ```
~~~

Such an installation could then be configured by having a rule:

``` {.dhall #gnuplot-rule}
let gnuplot =
    { match = Match.Class ["gnuplot", "figure"]
    , script = \(out : Text) -> \(inp : Text) -> ''
          gnuplot ${inp} > ${out}''
    }

in  { rules = [ gnuplot ]
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

# Watching the filesystem


