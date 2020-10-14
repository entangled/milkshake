-- ~\~ language=Dhall filename=test/Layer1/schema.dhall
-- ~\~ begin <<lit/index.md|test/Layer1/schema.dhall>>[0]
let List/map = https://prelude.dhall-lang.org/v11.1.0/List/map
    sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

-- ~\~ begin <<lit/index.md|milkshake-target>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/index.md|milkshake-action>>[0]
let Action : Type =
    { target : List Target
    , dependency : List Target
    , script : Optional Text
    }
-- ~\~ end

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
-- ~\~ end
