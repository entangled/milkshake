-- ~\~ language=Dhall filename=test/Layer2/test2.dhall
-- ~\~ begin <<docs/milkshake.md|test/Layer2/test2.dhall>>[0]
let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep
    sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let ms = ./schema.dhall

in  [ ms.fileRule "compile" (\(tgt : Text) -> \(deps : List Text) ->
        ''
        gcc -c ${Text/concatSep " " deps} -o ${tgt}
        '')
    , ms.fileRule "link" (\(tgt : Text) -> \(deps : List Text) ->
        ''
        gcc ${Text/concatSep " " deps} -o ${tgt}
        '')

    , ms.trigger "compile"
        [ ms.Target.File "hello.o" ]
        [ ms.Target.File "hello.c" ]
    , ms.trigger "link"
        [ ms.Target.File "hello" ]
        [ ms.Target.File "hello.o" ]

    , ms.fileAction "out.txt" [ "hello" ]
        ''
        ./hello > out.txt
        ''
    , ms.mainAction [ "out.txt" ]
    ] : List ms.Stmt
-- ~\~ end
-- ~\~ begin <<lit/milkshake.md|test/Layer2/test2.dhall>>[0]
let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep
    sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let ms = ./schema.dhall

in  [ ms.fileRule "compile" (\(tgt : Text) -> \(deps : List Text) ->
        ''
        gcc -c ${Text/concatSep " " deps} -o ${tgt}
        '')
    , ms.fileRule "link" (\(tgt : Text) -> \(deps : List Text) ->
        ''
        gcc ${Text/concatSep " " deps} -o ${tgt}
        '')

    , ms.trigger "compile"
        [ ms.Target.File "hello.o" ]
        [ ms.Target.File "hello.c" ]
    , ms.trigger "link"
        [ ms.Target.File "hello" ]
        [ ms.Target.File "hello.o" ]

    , ms.fileAction "out.txt" [ "hello" ]
        ''
        ./hello > out.txt
        ''
    , ms.mainAction [ "out.txt" ]
    ] : List ms.Stmt
-- ~\~ end
