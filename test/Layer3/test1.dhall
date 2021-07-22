-- ~\~ language=Dhall filename=test/Layer3/test1.dhall
-- ~\~ begin <<docs/milkshake.md|test/Layer3/test1.dhall>>[0]
let ms = ./schema.dhall

in  [ ms.fileAction "include.dhall" ([] : List Text)
        ''
        echo "./template.dhall 42" | dhall > include.dhall
        ''
    , ms.include "include.dhall"
    , ms.main ["answer.txt"]
    ]
-- ~\~ end
