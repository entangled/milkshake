-- ~\~ language=Dhall filename=test/Layer3/template.dhall
-- ~\~ begin <<lit/milkshake.md|test/Layer3/template.dhall>>[init]
let ms = ./schema.dhall

in \(x : Natural) ->
    [ ms.fileAction "answer.txt" ([] : List Text)
        ''
        echo "${Natural/show x}" > answer.txt
        ''
    ]
-- ~\~ end
