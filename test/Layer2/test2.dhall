-- ~\~ language=Dhall filename=test/Layer2/test2.dhall
-- ~\~ begin <<lit/index.md|test/Layer2/test2.dhall>>[0]
let Text/concatSep = https://prelude.dhall-lang.org/Text/concatSep
    sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let ms = ./schema.dhall

in
    { rules = toMap
        { compile = ms.fileRule (\(tgt : Text) -> \(deps : List Text) ->
            ''
            gcc -c ${Text/concatSep " " deps} -o ${tgt}
            '')
        , link = ms.fileRule (\(tgt : Text) -> \(deps : List Text) ->
            ''
            gcc ${Text/concatSep " " deps} -o ${tgt}
            '')
        }
    , triggers =
        [ { name = "compile", target = [ ms.Target.File "hello.o" ]
          , dependency = [ ms.Target.File "hello.c" ] }
        , { name = "link", target = [ ms.Target.File "hello" ]
          , dependency = [ ms.Target.File "hello.o" ] }
        ] : List ms.Trigger
    , actions =
        [ ms.file "out.txt" [ "hello" ]
            ''
            ./hello > out.txt
            ''
        , ms.main [ "out.txt" ]
        ]
    }
-- ~\~ end
