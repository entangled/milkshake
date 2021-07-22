-- ~\~ language=Dhall filename=test/Layer3/template.dhall
-- ~\~ begin <<docs/milkshake.md|test/Layer3/template.dhall>>[0]
let ms = ./schema.dhall

in \(x : Natural) ->
    [ ms.fileAction "answer.txt" ([] : List Text)
        ''
        echo "${Natural/show x}" > answer.txt
        ''
    ]
-- ~\~ end
