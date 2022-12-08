-- ~\~ language=Dhall filename=test/Layer4/schema.dhall
-- ~\~ begin <<lit/milkshake.md|final-schema>>[init]
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

-- ~\~ begin <<lit/milkshake.md|milkshake-target>>[init]
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
-- ~\~ begin <<lit/milkshake.md|milkshake-action>>[init]
let Dependency = \(Tgt : Type) -> \(Dep : Type) ->
    { target : Tgt
    , dependency : Dep
    }

let Action : Type =
    { script : Optional Text
    } //\\ (Dependency (List Target) (List Target))
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|milkshake-trigger>>[init]
let Call : Type =
    { name : Text
    } //\\ (Dependency (List Target) (List Target))
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|milkshake-rule>>[init]
let Generator : Type =
    List Target -> List Target -> Optional Text
let Rule : Type =
    { name : Text
    , gen : Generator
    }
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|milkshake-stmt>>[init]
let Watch : Type =
    { paths : List Text
    , target : Target
    }

let Stmt : Type =
    < Action  : Action
    | Rule    : Rule
    | Call : Call
    | Include : Text
    | Watch   : Watch
    | Main    : List Text >

let action = \(tgt : List Target) -> \(dep : List Target) -> \(script : Optional Text) ->
    Stmt.Action { target = tgt, dependency = dep, script = script }
let rule = \(name : Text) -> \(gen : Generator) ->
    Stmt.Rule { name = name, gen = gen }
let call = \(name : Text) -> \(tgt : List Target) -> \(dep : List Target) ->
    Stmt.Call { name = name, target = tgt, dependency = dep }
let include = Stmt.Include
let main = Stmt.Main
let watch = \(paths : List Text) -> \(tgt : Target) ->
    Stmt.Watch { paths = paths, target = tgt }
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|milkshake-convenience>>[init]
let fileName = \(a : Target) ->
    merge { File = \(x : Text) -> Some x
          , Generic = \(_ : Virtual) -> None Text
          , Phony =   \(_ : Text) -> None Text } a

let Target/isFile = \(a : Target) ->
    merge { File = \(_ : Text) -> True
          , Generic = \(_ : Virtual) -> False
          , Phony = \(_ : Text) -> False } a

let getFiles = \(a : List Target) ->
    Prelude.List.unpackOptionals Text (List/map Target (Optional Text) fileName a)

let testGetFiles = assert : getFiles [ Target.File "a", Target.Phony "m", Target.File "b" ]
                        === [ "a", "b"]

let fileRule = \(name : Text) -> \(f : Text -> List Text -> Text) ->
     rule name (\(tgt : List Target) -> \(dep : List Target) ->
        merge { Some = \(inp : Text) -> Some (f inp (getFiles dep))
            , None = None Text } (List/head Text (getFiles tgt)))
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|milkshake-convenience>>[1]
let fileAction = \(target : Text) -> \(deps : List Text) -> \(script : Text) ->
    Stmt.Action
        { target = [ Target.File target ]
        , dependency = List/map Text Target Target.File deps
        , script = Some script }

let fileCall = \(name : Text) -> \(tgt : Text) -> \(deps : List Text) ->
    call name [Target.File tgt] (List/map Text Target Target.File deps)

let mainAction = \(deps : List Text) ->
    Stmt.Action
        { target = [ Target.Phony "main" ]
        , dependency = List/map Text Target Target.File deps
        , script = None Text }
-- ~\~ end

in  { Stmt = Stmt
    , Target = Target, action = action, rule = rule, call = call
    , include = include, main = main
    , fileName = fileName
    , getFiles = getFiles
    , fileRule = fileRule
    , fileAction = fileAction, fileCall = fileCall
    , mainAction = mainAction, watch = watch
    }
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|final-schema>>[1]

-- ~\~ end
