-- ~\~ language=Dhall filename=test/Layer2/schema.dhall
-- ~\~ begin <<lit/index.md|test/Layer2/schema.dhall>>[0]
let Prelude = https://prelude.dhall-lang.org/v19.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2
let List/map = Prelude.List.map
let Text/concatSep = Prelude.Text.concatSep
let Map/Type = Prelude.Map.Type
let Map/Entry = Prelude.Map.Entry
-- let List/map = https://prelude.dhall-lang.org/v11.1.0/List/map
--     sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
-- let List/unpackOptionals = https://prelude.dhall-lang.org/v11.1.0/List/unpackOptionals
--     sha256:0cbaa920f429cf7fc3907f8a9143203fe948883913560e6e1043223e6b3d05e4
-- ~\~ begin <<lit/index.md|milkshake-target>>[0]
let Content : Type =
    { name : Text
    , exists : Text
    , hash : Text
    }

let Target : Type =
    < File : Text
    | Generic : Content
    | Phony : Text
    >
-- ~\~ end
-- ~\~ begin <<lit/index.md|milkshake-action>>[0]
let Dependency = \(Tgt : Type) -> \(Dep : Type) ->
    { target : Tgt
    , dependency : Dep
    }

let Action : Type =
    { script : Optional Text
    } //\\ (Dependency (List Target) (List Target))
-- ~\~ end
-- ~\~ begin <<lit/index.md|milkshake-trigger>>[0]
let Trigger : Type =
    { name : Text
    } //\\ (Dependency (List Target) (List Target))
-- ~\~ end
-- ~\~ begin <<lit/index.md|milkshake-rule>>[0]
let Rule : Type = List Target -> List Target -> Optional Text
-- ~\~ end
let fileName = \(a : Target) ->
    merge { File = \(x : Text) -> Some x
          , Generic = \(_ : Content) -> None Text
          , Phony =   \(_ : Text) -> None Text } a

let Target/isFile = \(a : Target) ->
    merge { File = \(_ : Text) -> True
          , Generic = \(_ : Content) -> False
          , Phony = \(_ : Text) -> False } a

let getFiles = \(a : List Target) ->
    Prelude.List.unpackOptionals Text (List/map Target (Optional Text) fileName a)

let testGetFiles = assert : getFiles [ Target.File "a", Target.Phony "m", Target.File "b" ]
                        === [ "a", "b"]

let fileRule = \(f : Text -> List Text -> Text) ->
     \(tgt : List Target) -> \(dep : List Target) ->
     merge { Some = \(inp : Text) -> Some (f inp (getFiles dep))
           , None = None Text } (List/head Text (getFiles tgt))

let exampleRule1 = fileRule (\(tgt : Text) -> \(deps : List Text) -> 
                             "gcc ${Text/concatSep " " deps} -o ${tgt}")
let testFileRule = assert : exampleRule1 [ Target.File "hello" ] [ Target.File "hello.c" ]
                 === Some "gcc hello.c -o hello"

let file = \(target : Text) -> \(deps : List Text) -> \(script : Text) ->
    { target = [ Target.File target ]
    , dependency = List/map Text Target Target.File deps
    , script = Some script } : Action

let main = \(deps : List Text) ->
    { target = [ Target.Phony "main" ]
    , dependency = List/map Text Target Target.File deps
    , script = None Text } : Action

in  { Target = Target
    , Action = Action
    , Trigger = Trigger
    , Rule = Rule
    , fileName = fileName
    , getFiles = getFiles
    , fileRule = fileRule
    , file = file
    , main = main
    }
-- ~\~ end
