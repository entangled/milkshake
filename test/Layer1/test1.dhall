-- ~\~ language=Dhall filename=test/Layer1/test1.dhall
-- ~\~ begin <<lit/milkshake.md|test/Layer1/test1.dhall>>[init]
let ms = ./schema.dhall

in [ ms.file "hello" [ "hello.c" ]
      ''
      gcc hello.c -o hello
      ''
   , ms.file "out.txt" [ "hello" ]
      ''
      ./hello > out.txt
      ''
   , ms.main [ "out.txt" ]
   ]
-- ~\~ end
