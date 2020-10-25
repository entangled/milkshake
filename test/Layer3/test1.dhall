-- ~\~ language=Dhall filename=test/Layer3/test1.dhall
-- ~\~ begin <<lit/index.md|test/Layer3/test1.dhall>>[0]
let ms = ./schema.dhall

in  [ ms.fileAction "include.dhall" ([] : List Text)
        ''
        dhall <<< "./template.dhall 42" > include.dhall
        ''
    , ms.include "include.dhall"
    , ms.main ["answer.txt"]
    ]
-- ~\~ end
