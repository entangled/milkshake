-- ~\~ language=Dhall filename=test/basic/rules.dhall
-- ~\~ begin <<lit/milkshake.md|test/basic/rules.dhall>>[init]
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
-- ~\~ end
