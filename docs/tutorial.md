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

