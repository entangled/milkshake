let ms = ../../data/Milkshake.dhall
let Text/concat = https://prelude.dhall-lang.org/Text/concat

in [ ms.fileRule "rot13" (\(tgt : Text) -> \(deps : List Text) -> ''
         tr a-zA-Z n-za-mN-ZA-M < ${Text/concat deps} > ${tgt}
         '')
   , ms.fileCall "rot13" "secret.txt" ["message.txt"]
   , ms.watch ["message.txt"] (ms.Target.File "secret.txt")
]
