let ms = ../../data/Milkshake.dhall
-- https://raw.githubusercontent.com/entangled/milkshake/master/data/Milkshake.dhall
let Text/concat = https://prelude.dhall-lang.org/Text/concat

in [ ms.fileAction "secret.txt" ([] : List Text) ''
         echo "Uryyb, Jbeyq!" > secret.txt
         ''
   , ms.fileRule "rot13" (\(tgt : Text) -> \(deps : List Text) -> ''
         tr a-zA-Z n-za-mN-ZA-M < ${Text/concat deps} > ${tgt}
         '')
   , ms.fileCall "rot13" "message.txt" ["secret.txt"]
   , ms.mainAction ["message.txt"]
]
