# Tutorial
Milkshake uses the Dhall language to specify dependencies between actions. In the most basic operation, we have `Target` and `Action`. A `Target` is usually one or more files, while an `Action` is a shell script that would generate said `Target`, after satisfying a set of dependencies (also `Target`).

The resulting configuration is a list of `Action`. Since we will add other forms of *statements* later on, we use some Dhall functions to write out actions. For example, to create a file `hello.txt` and consequently put it in all caps, could be done as follows:

``` {.dhall file=say-hello.dhall}
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in [ ms.fileAction "hello.txt" ([] : List Text) ''
         echo "Hello, World" > hello.txt
         ''
   , ms.fileAction "HELLO.TXT" ["hello.txt"] ''
         tr a-z A-Z < hello.txt > HELLO.TXT
         ''
   , ms.mainAction ["HELLO.TXT"]
]
```

``` {.bash .eval}
milkshake -1 ./say-hello.dhall
```

## Rules
Because it is not so nice to write out every action explicitely, we can define rules. A rule is a function from input targets to output targets. A rule can be triggered by calling it:

``` {.dhall file=hello-again.dhall}
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall
let Text/concat = https://prelude.dhall-lang.org/Text/concat

in [ ms.fileAction "secret.txt" ([] : List Text) ''
         echo "Uryyb, Jbeyq!" > secret.txt
         ''
   , ms.fileRule "rot13" (\(tgt : Text) -> \(deps : List Text) -> ''
         tr a-zA-Z n-za-mN-ZA-M < ${Text/concat deps} > ${tgt}
         '')
   , ms.fileCall "rot13" "message.txt" ["secret.txt"]
   , ms.mainAction ["message.txt"]
]
```

## Includes
Includes allow one to incorporate the contents of one Milkshake file into an other. The nice thing is that the include can be the result of an action. As an example we have here a template that takes a number as an argument:

``` {.dhall file=template.dhall}
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in \(x : Natural) ->
    [ ms.fileAction "answer.txt" ([] : List Text)
        ''
        echo "${Natural/show x}" > answer.txt
        ''
    ]
```

The main file imports the generated include

``` {.dhall file=include.dhall}
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in  [ ms.fileAction "include.dhall" ([] : List Text)
        ''
        dhall <<< "./template.dhall 42" > include.dhall
        ''
    , ms.include "include.dhall"
    , ms.main ["answer.txt"]
    ]
```

## Watches
Milkshake can set a number of watches to trigger a file to be built.

``` {.dhall file=write-secret.dhall}
let ms = https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall

in [ ms.fileRule "rot13" (\(tgt : Text) -> \(deps : List Text) -> ''
         tr a-zA-Z n-za-mN-ZA-M < ${Text/concat deps} > ${tgt}
         '')
   , ms.fileCall "rot13" "secret.txt" ["message.txt"]
   , ms.watch ["message.txt"] (ms.Target.File "secret.txt")
]
```

Now, run:

``` {.bash}
milkshake ./wait.dhall
```

This will always keep your encoded message up-to-date!