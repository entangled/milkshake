let ms = ../../data/Milkshake.dhall

in [ ms.fileAction "secret.txt" ([] : List Text) ''
         echo "Uryyb, Jbeyq!" > secret.txt
         ''
   , ms.fileAction "message.txt" ["secret.txt"] ''
         tr a-zA-Z n-za-mN-ZA-M < secret.txt > message.txt
         ''
   , ms.mainAction ["message.txt"]
]
