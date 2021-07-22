-- ~\~ language=Dhall filename=test/Layer1/schema.dhall
-- ~\~ begin <<lit/milkshake.md|test/Layer1/schema.dhall>>[0]
let Prelude = https://prelude.dhall-lang.org/v19.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2
let List/map = Prelude.List.map
let Text/concatSep = Prelude.Text.concatSep

-- ~\~ begin <<lit/milkshake.md|milkshake-target>>[0]
let Virtual : Type =
    { name : Text
    , exists : Text    -- Script to check existence
    , content : Text   -- Script to read content
    }

let Target : Type =
    < File : Text
    | Generic : Virtual
    | Phony : Text
    >
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|milkshake-action>>[0]
let Dependency = \(Tgt : Type) -> \(Dep : Type) ->
    { target : Tgt
    , dependency : Dep
    }

let Action : Type =
    { script : Optional Text
    } //\\ (Dependency (List Target) (List Target))
-- ~\~ end

let file = \(target : Text) -> \(deps : List Text) -> \(script : Text) ->
    { target = [ Target.File target ]
    , dependency = List/map Text Target Target.File deps
    , script = Some script }

let main = \(deps : List Text) ->
    { target = [ Target.Phony "main" ]
    , dependency = List/map Text Target Target.File deps
    , script = None Text }

in { Target = Target
   , Action = Action
   , Virtual = Virtual
   , file = file
   , main = main }
-- ~\~ end
